# Problem: D_2x2_Erasing_2.pas

```pascal
program D_2x2_Erasing_2;
{$MODE DELPHI}
uses
	math;
const
	hh = 7;
var
	ntc, tci, h, w, i, ans: int8;
	s: array [1 .. hh] of string;

function isBlack(i, j: int8): boolean;
begin
	if (i > h) or (j > w) then
		Result := false
	else
		Result := (s[i-1][j-1] = '#') and (s[i-1][j] = '#')
			and (s[i][j-1] = '#') and (s[i][j] = '#');
end;

procedure dfs(i, j, paints: int8);
begin
	if j > w then begin
		j := 2;
		inc(i);
	end;
	if i > h then
		ans := min(ans, paints)
	else if isBlack(i, j) then begin
		s[i][j] := '.';
		dfs(i, j+1, paints+1);
		s[i][j] := '#';
	end else if isBlack(i, j+1) and isBlack(i+1, j) then begin
		dfs(i, j+1, paints);
		s[i][j] := '.';
		dfs(i, j+1, paints + 1);
		s[i][j] := '#';
	end else
		dfs(i, j+1, paints);
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(h, w);
		for i := 1 to h do readln(s[i]);

		ans := 50;
		dfs(2, 2, 0);
		writeln(ans);

	end;
end.

```
