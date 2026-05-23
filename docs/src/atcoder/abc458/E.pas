program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp #unsolved
uses
	generics.collections;
const
	XX = 100;
	PRIME = 998244353;
var
	x1, x2, x3, i1, i2, i3: int32;
	dp1, dp2, dp3: array [-1 .. XX, -1 .. XX+1, -1 .. XX] of uint32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure modinc(var a: int32; b: int32);
begin
	inc(a, b);
	if a >= prime then dec(a, prime);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x1, x2, x3);

	dp1[-1, -1, -1] := 0;
	dp2[-1, -1, -1] := 0;
	dp3[-1, -1, -1] := 0;

	dp1[0, 0, 0] := 0;
	dp2[0, 0, 0] := 1;
	dp3[0, 0, 0] := 0;

	for i1 := 0 to x1 do
	for i2 := 0 to x2+1 do
	for i3 := 0 to x3 do
	if i1 + i2 + i3 > 0 then begin
		dp1[i1, i2, i3] := dp1[i1-1, i2, i3] + dp2[i1, i2-1, i3];
		dp2[i1, i2, i3] := dp1[i1-1, i2, i3] + dp2[i1, i2-1, i3] + dp3[i1, i2, i3-1];
		dp3[i1, i2, i3] := dp2[i1, i2-1, i3] + dp3[i1, i2, i3-1];
		if dp1[i1, i2, i3] >= PRIME then dec(dp1[i1, i2, i3], PRIME);
		while dp2[i1, i2, i3] >= PRIME do dec(dp2[i1, i2, i3], PRIME);
		if dp3[i1, i2, i3] >= PRIME then dec(dp3[i1, i2, i3], PRIME);
	end;

	writeln(dp2[x1, x2+1, x3]);
end.
