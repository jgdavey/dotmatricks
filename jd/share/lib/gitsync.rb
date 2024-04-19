require 'open3'
require 'ostruct'

class Gitsync
  module Run
    extend self

    Result = Struct.new(:success?, :out, :err)

    def capture(cmd, **opts)
      out, err, status = Open3.capture3(*cmd, **opts)
      Result.new(status.success?, out.to_s.chomp, err.to_s.chomp)
    end

    def run(cmd, **opts)
      result = capture(cmd, **opts)
      if result.success?
        result.out
      else
        $stderr.puts "ERROR running #{cmd.join(" ")} (#{opts[:chdir].to_s})"
        $stderr.puts result.out
        $stderr.puts result.err
      end
    end
  end

  Ref = Struct.new(:name, :long, :sha)

  Diff = Struct.new(:local, :remote) do
    def equal?
      local.sha == remote.sha
    end

    def range
      "#{local.sha[0..7]}..#{remote.sha[0..7]}"
    end
  end

  attr_reader :quiet, :current_branch, :tracking_diffs

  def initialize(quiet: false, dir: Dir.pwd, **)
    @dir = dir
    fetch = ["git", "fetch", "--progress", "--all", "-P", "-p"] + (quiet ? ["--quiet"] : [])
    run *fetch
    @quiet = quiet
    @default_branch = begin
                        db = run "git", "config", "init.defaultbranch"
                        db = "main" if db.empty?
                        db
                      end
    @current_branch = run "git", "rev-parse", "--abbrev-ref", "HEAD"
    @is_clean_working = run("git", "status", "--porcelain").lines.reject { |line| line.start_with? "??" }.empty?
    @tracking_diffs = run("git", "for-each-ref", "--format", "%(refname:short):%(upstream:short)", "refs/heads")
                        .lines
                        .map(&:chomp)
                        .reject { |line| line.end_with?(":") }
                        .map { |line|
      l, r = line.split(":", 2)
      local = Ref.new(l, "refs/heads/#{l}", run("git", "rev-parse", "-q", "refs/heads/#{l}"))
      remote_long = "refs/remotes/#{r}"
      remote_result = Run.capture(["git", "rev-parse", "-q", remote_long], chdir: @dir)
      remote = if remote_result.success?
                 Ref.new(r, remote_long, remote_result.out)
               else
                 :gone
               end
      Diff.new(local, remote)
    }
  end

  def run(*cmd)
    Run.run cmd, chdir: @dir
  end

  Action = Struct.new(:branch, :message) do
    def color; end
    def formatted(colorize = false)
      if colorize
        "#{color}#{message}\033[0m"
      else
        message
      end
    end
  end

  class Noop < Action
    def color
      "\033[32m" # green
    end
  end

  class Warning < Action
    def color
      "" # default
    end
  end

  class Error < Action
    def color
      "\033[31;1m" # light red
    end
  end

  class Update < Action
    attr_reader :command
    def initialize(branch, message, command)
      super(branch, message)
      @command = command
    end

    def color
      "\033[32;1m" # light green
    end
  end

  def ancestor?(sha1, sha2)
    Run.capture("git merge-base --is-ancestor #{sha1} #{sha2}", chdir: @dir).success?
  end

  def actions
    tracking_diffs.map do |diff|
      if diff.remote == :gone
        if ancestor?(diff.local.sha, @default_branch)
          # Gone, but merged
          if diff.local.name == current_branch
            Warning.new(diff.local.name, "WARN: Remote branch is gone, but still checked out locally")
          else
            Update.new(diff.local.name, "deleted, was #{diff.local.sha}", ["git", "branch", "-D", diff.remote.name] + (@quiet ? ["--quiet"] : []))
          end
        else
          Error.new(diff.local.name,  "ERROR: Remote branch is gone, but not merged locally")
        end
      elsif diff.remote.sha.nil? || diff.remote.sha.empty?
        Noop.new(diff.local.name, "no remote")
      elsif diff.equal?
        Noop.new(diff.local.name, "up to date")
      elsif ancestor?(diff.local.sha, diff.remote.sha)
        if diff.local.name == current_branch
          if @is_clean_working
            Update.new(diff.local.name, "updated #{diff.range}", ["git", "merge", "--ff-only", diff.remote.name] + (@quiet ? ["--quiet"] : []))
          else
            Error.new(diff.local.name, "ERROR: #{diff.local.name} has files that are not checked in")
          end
        else
          Update.new(diff.local.name, "updated #{diff.range}", ["git", "update-ref", diff.local.long, diff.remote.long])
        end
      else
        Warning.new(diff.local.name, "WARN: #{diff.local.name} has unpushed commits")
      end
    end
  end
end
