program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 200 * 1000;
var
	n, i, ans0, ans1: int32;
	b, d: int8;
	a0, a1: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		read(a0[i]);
		a1[i] := a0[i];
	end;
	readln;

	ans0 := 0;
	ans1 := 1;
	a1[1] := a1[1] xor 1;

	for i := 1 to n-1 do begin
		read(b);

		d := a0[i] xor a0[i+1] xor b;
		inc(ans0, d);
		a0[i+1] := a0[i+1] xor d;

		d := a1[i] xor a1[i+1] xor b;
		inc(ans1, d);
		a1[i+1] := a1[i+1] xor d;
	end;
	readln;

	writeln(min(ans0, ans1));
end.
