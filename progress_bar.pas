{*
 * Altium to KiCad schematic library converter script
 *
 * Copyright (C) 2017 CERN
 * @author Maciej Suminski <maciej.suminski@cern.ch>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you may find one here:
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * or you may search the http://www.gnu.org website for the version 2 license,
 * or you may write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 *}

unit ProgressBarDialog;

interface

type
TProgressBarForm = class(TForm)
  ProgressBar : TProgressBar;
  statusLabel : TLabel;
end;

var
  progressDlg : TProgressBarForm;

implementation

{$R *.DFM}

procedure ProgressInit(aCaption : TDynamicString; aMax : Integer);
begin
    progressDlg.statusLabel.Caption := aCaption;
    progressDlg.ProgressBar.Max     := aMax;

    progressDlg.Show;
end;


procedure ProgressUpdate(aIncrement : Integer);
begin
    if progressDlg.ProgressBar.Position < progressDlg.ProgressBar.Max then
    begin
        progressDlg.ProgressBar.Position := ProgressBar.Position + aIncrement;
        progressDlg.Refresh;
    end;
end;


procedure ProgressFinish(aDummy : Integer);
begin
    progressDlg.statusLabel.Visible  := true;
    progressDlg.Hide;
end;

end.
