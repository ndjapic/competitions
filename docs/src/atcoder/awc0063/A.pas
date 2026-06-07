program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #grid
const
	hh = 1000;
var
	h, w, r, c, k: int32;
	s: array [1 .. hh] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	k := 0;
	for r := 1 to h do begin
		readln(s[r]);
		for c := 1 to w do
			if s[r][c] = 'T' then inc(k);
	end;

	writeln(k);

	for r := 1 to h do
		for c := 1 to w do
			if s[r][c] = 'T' then writeln(r, ' ', c);
end.
