# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "ical2org";
  overlay =
    {}:
    final:
    prev:
    {
      x-wr-timezone = with prev;
        python3.pkgs.buildPythonPackage rec {
          pname = "x_wr_timezone";
          version = "0.0.5";

          src = python3.pkgs.fetchPypi {
            inherit pname version;
            sha256 = "sha256-wFyzS5tYpGB6eI2whtyuV2ZyjkuU4GcocNxVk6bhP+Y=";
          };

          propagatedBuildInputs = with python3.pkgs; [
            pytz
            icalendar
            pygments
            restructuredtext_lint
            pytest
          ];

          meta = with lib; {};
        };

      recurring-ical-events = with prev;
        python3.pkgs.buildPythonPackage rec {
          pname = "recurring_ical_events";
          version = "1.0.2b0";

          src = python3.pkgs.fetchPypi {
            inherit pname version;
            sha256 = "sha256-aoQU7rxRJvqe3PLHPto5T2rCvFSkmqfiClwCL6SRjk0=";
          };

          propagatedBuildInputs = with python3.pkgs; [
            pytz
            dateutil
            final.x-wr-timezone
            tzdata
            pytest-cov
            pbr
          ];

          meta = with lib; {};
        };

      ical2orgpy = with prev;
        python3.pkgs.buildPythonApplication rec {
          pname = "ical2orgpy";
          version = "0.4.0";

          src = python3.pkgs.fetchPypi {
            inherit pname version;
            sha256 = "sha256-7/kWW1oTSJXPJtN02uIDrFdNJ9ExKRUa3tUNA0oJSoc=";
          };

          propagatedBuildInputs = with python3.pkgs; [
            click
            future
            icalendar
            pytz
            tzlocal
            final.recurring-ical-events
          ];

          meta = with prev.lib; {};
        };
    };
}
