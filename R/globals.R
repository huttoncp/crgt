# Copyright 2023 Craig Hutton
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# global variable definitions ---------------------------------------------
utils::globalVariables(c("session", "start_date", "session_date", "n_trials", "subject",
                         "total_choices", "prematures", "sum_of_prematures", "offered", "trial",
                         "n_trials", "session", "stim_dur", "prop_prems", "prop_premscam",
                         "premature_resp", "premature_hole",  "premat_n", "premat_prop",
                         "msn", "chosen", "omission", "omissions",
                         "correct", "n_correct", "n_incorrect", "prop_correct", "stage", "criterion_met",
                         "final_criterion_met", "contr.sum", "group",
                         "collect_lat", "mean_collect_lat", "mean_choice_lat",
                         "times_chosen", "choice_prop", "P1", "P2", "P3", "P4", "choice_score",
                         "choice_lat_P1", "choice_lat_P2", "choice_lat_P3", "choice_lat_P4",
                         "%+replace%", "rel", "text_family",
                         "choice", "condval", "condsd", "term", "grp", "grpvar", "OR", "OR_LB", "OR_UB", "unstable"))
