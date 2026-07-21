program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 5000;
var
	n, m, i, j, k, a: int32;
	dp: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function modmul(a, b: int32): int32;
begin
	modmul := int64(a) * b mod m;
end;

function modpow(b, e: int32): int32;
begin
	if e = 0 then
		modpow := 1
	else if odd(e) then
		modpow := modmul(b, modpow(b, e-1))
	else
		modpow := modpow(modmul(b, b), e div 2);
end;

procedure modinc(var a: int32; b: int32);
begin
	inc(a, b);
	if a >= m then dec(a, m);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, m);

	for j := 1 to k do dp[j] := 0;
	dp[0] := 1;

	for i := 1 to n do begin
		read(a);
		a := a mod m;
		if (m > 2) and (a > 0) then a := modpow(a, m-2);

		for j := min(i, k) downto 1 do
			modinc(dp[j], modmul(dp[j-1], a));
	end;
	readln;

	writeln(dp[k]);
end.
