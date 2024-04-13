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
      out, err, status = Open3.capture3(*cmd, **opts)
      if status.success?
        out.to_s.chomp
      else
        $stderr.puts "ERROR running #{cmd.join(" ")} (#{opts[:chdir].to_s})"
        $stderr.puts out
        $stderr.puts err
      end
    end
  end

  Ref = Struct.new(:name, :long, :sha)

  Diff = Struct.new(:local, :remote) do
    def equal?
      local.sha == remote.sha
    end

    def range
      "#{local.sha[0..8]}..#{remote.sha[0..8]}"
    end
  end

  attr_reader :quiet, :default_branch, :current_branch, :tracking_diffs

  def initialize(quiet: false, dir: Dir.pwd, **)
    @dir = dir
    run *["git", "fetch", "--progress", "--all", "-P", "-p"] + (quiet ? ["--quiet"] : [])
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
      remote = Ref.new(r, "refs/remotes/#{r}", run("git", "rev-parse", "-q", "refs/remotes/#{r}"))
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
        "(\033[1m#{branch}\033[0m) #{color}#{message}\033[0m"
      else
        "(#{branch}) #{message}"
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
    attr_reader :range, :command
    def initialize(branch, range, command)
      super(branch, "update")
      @range = range
      @command = command
    end

    def color
      "\033[32;1m" # light green
    end

    def formatted(colorize = false)
      super(colorize) + " #{range}"
    end
  end

  def ancestor?(sha1, sha2)
    Run.capture("git merge-base --is-ancestor #{sha1} #{sha2}", chdir: @dir).success?
  end

  def actions
    tracking_diffs.map do |diff|
      if diff.remote.sha.empty?
        Noop.new(diff.local.name, "no remote")
      elsif diff.equal?
        Noop.new(diff.local.name, "up to date")
      elsif ancestor?(diff.local.sha, diff.remote.sha)
        if diff.local.name == current_branch
          if @is_clean_working
            Update.new(diff.local.name, diff.range, ["git", "merge", "--ff-only", diff.remote.name] + (@quiet ? ["--quiet"] : []))
          else
            Error.new(diff.local.name, "ERROR: #{diff.local.name} has files that are not checked in")
          end
        else
          Update.new(diff.local.name, diff.range, ["git", "update-ref", diff.local.full, diff.remote.full])
        end
      else
        Warning.new(diff.local.name, "WARN: #{diff.local.name} has unpushed commits")
      end
    end
  end
end
