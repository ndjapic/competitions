program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #grid #color #rectangle
uses
	math;
const
	HH = 1000;
var
	h, w, a, b, c, d, i, j: int32;
	ans: boolean;
	s: array [1 .. HH] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	a := h;
	b := 1;
	c := w;
	d := 1;

	for i := 1 to h do begin
		readln(s[i]);
		for j := 1 to w do
			if s[i][j] = '#' then begin
				a := min(a, i);
				b := max(b, i);
				c := min(c, j);
				d := max(d, j);
			end;
	end;

	ans := true;
	for i := a to b do
		if ans then
			for j := c to d do
				ans := ans and (s[i][j] <> '.');

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
