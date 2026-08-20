program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	notc, tci, n, i: int32;
	e: int8;
	a, b: array [1 .. NN] of int8;
	c: array [0 .. 3] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		for i := 1 to n do read(b[i]);
		readln;

		for e := 0 to 3 do c[e] := 0;

		for i := 1 to n do
			inc(c[ 2 *  a[i] + b[i] ]);

		if c[1] + c[2] = 0 then
			writeln('0')
		else if odd(c[2]) then
			writeln('1')
		else if c[2] + c[3] = 0 then
			writeln('-1')
		else if c[0] + c[2] = 0 then
			writeln('-1')
		else
			writeln('2');

	end;
end.
