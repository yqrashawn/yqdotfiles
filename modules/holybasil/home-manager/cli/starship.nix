{ config, lib, pkgs, ... }:

{
  programs.starship = {
    enable = true;
    settings = {
      add_newline = true;
      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$localip"
        "$shlvl"
        "$singularity"
        "$kubernetes"
        "$directory"
        "$vcsh"
        "$git_branch"
        "$git_commit"
        "$git_state"
        "$git_metrics"
        "$git_status"
        "$hg_branch"
        "$docker_context"
        "$package"
        "$cmake"
        "$cobol"
        "$container"
        "$dart"
        "$deno"
        "$dotnet"
        "$elixir"
        "$elm"
        "$erlang"
        "$golang"
        "$helm"
        "$java"
        "$julia"
        "$kotlin"
        "$lua"
        "$nim"
        "$nodejs"
        "$ocaml"
        "$perl"
        "$php"
        "$pulumi"
        "$purescript"
        "$python"
        "$rlang"
        "$red"
        "$ruby"
        "$rust"
        "$scala"
        "$swift"
        "$terraform"
        "$vlang"
        "$vagrant"
        "$zig"
        "$nix_shell"
        "$conda"
        "$memory_usage"
        "$aws"
        "$gcloud"
        "$openstack"
        "$azure"
        "$env_var"
        "$crystal"
        "$custom"
        "$sudo"
        "$cmd_duration"
        "$fill"
        "$battery"
        "$time"
        "$line_break"
        "$jobs"
        "$status"
        "$shell"
        "$character"
      ];
      fill.symbol = "⠁ ";
      character = {
        success_symbol = "[❯](bold green)";
        error_symbol = "[❯](bold red)";
        vicmd_symbol = "[❮](bold blue)";
      };
      directory = {
        truncation_length = 0;
        truncate_to_repo = true;
      };
      status = {
        # format =
        #   "[[$symbol $status $common_meaning$signal_name$maybe_int]]($style) ";
        symbol = "✖ ";
        disabled = false;
        map_symbol = true;
        pipestatus = true;
      };
      sudo.disabled = false;
      localip.disabled = false;
      shell = {
        disabled = false;
        style = "bold";
      };
      nodejs = {
        style = "green";
        symbol = " ";
      };
      java.symbol = " ";
      aws.symbol = "  ";
      gcloud.symbol = "  ";
      git_status = {
        conflicted = "conflicted=";
        ahead = "⇡";
        behind = "⇣";
        diverged = "diverged⇕";
        up_to_date = "";
        untracked = "untracked?";
        renamed = "renamed»";
        deleted = "deleted✘";
      };
      time.disabled = false;
      custom = {
        babashka = {
          files = [ "bb.edn" ];
          command = "bb version";
        };
      };
    };
  };
}
