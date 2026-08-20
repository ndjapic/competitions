program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HH = 20;
var
	h, w, i, j: int8;
	ans: boolean;
	s: array [1 .. HH] of string;
	c: array [1 .. HH, 1 .. HH] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do begin
		readln(s[i]);
		for j := 1 to w do c[i, j] := 0;
	end;

	for i := 1 to h do
		for j := 2 to w do
			if (s[i][j-1] = '#') and (s[i][j] = '#') then begin
				inc(c[i, j-1]);
				inc(c[i, j]);
			end;

	for i := 2 to h do
		for j := 1 to w do
			if (s[i-1][j] = '#') and (s[i][j] = '#') then begin
				inc(c[i-1, j]);
				inc(c[i, j]);
			end;

	ans := true;
	for i := 1 to h do
		for j := 1 to w do
			if ans and (s[i][j] = '#') then ans := (c[i, j] = 2) or (c[i, j] = 4);

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
