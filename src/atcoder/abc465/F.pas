program _F;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
const
	XX = 1000 * 1000;
var
	n, i, x: int32;
	e: int8;
	ch: char;
	s: string;
	v: array [0 .. XX] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for x := 0 to XX do v[x] := 0;

	for i := 1 to n do begin
		x := 0;
		for e := 5 downto 0 do begin
			read(ch);
			x := 10 * x + ord(ch) - ord('0');
		end;
		readln(v[x+1]);
	end;

	for x := 0 to XX-1 do inc(v[x+1], v[x]);

end.
