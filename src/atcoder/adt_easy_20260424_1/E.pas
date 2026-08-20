program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	hh = 1000;
var
	h, w, i, j, a, b, c, d: int32;
	found: boolean;
	s: array [1 .. hh] of string;
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

	found := false;
	for i := a to b do
		for j := c to d do
			found := found or (s[i, j] = '.');

	if found then
		writeln('No')
	else
		writeln('Yes');
end.
