{ pkgs, secret, ... }:
let
in
{
  systemd.services.ical-vu-sync = {
    serviceConfig.Type = "oneshot";
    path = with pkgs; [ bash ical2orgpy curl ];
    script = ''
      rm "${secret.ical2org.orgPath}"
      cat <<EOF > "${secret.ical2org.orgPath}"
      :PROPERTIES:
      :ID:       56ed0bf0-c6d0-4a86-980a-905ccab89345
      :END:
      #+title: VU Calendar
      #+filetags: :project-forced:
      EOF
      curl '${secret.ical2org.icalUrl}' -o - | ical2orgpy - - >> "${secret.ical2org.orgPath}"
      chown 404:404 "${secret.ical2org.orgPath}"
    '';
  };
  systemd.timers.ical-vu-sync = {
    wantedBy = [ "timers.target" ];
    partOf = [ "ical-vu-sync.service" ];
    timerConfig = {
      OnCalendar = "*-*-* 3:00:00";
      Unit = "ical-vu-sync.service";
    };
  };
}
