program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	prime = 1000 * 1000 * 1000 + 7;
var
	n, i, s, mx: int32;
	k, total: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function modmul(a, b: int32): int32;
begin
	modmul := int64(a) * b mod prime;
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

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	mx := 0;
	total := 0;
	for i := 1 to n do begin
		read(s);
		inc(total, s);
		mx := max(mx, s);
	end;

	dec(total, mx);
	mx := modmul(mx, modpow(2, k mod (prime - 1)));
	inc(total, mx);
	writeln(total mod prime);
end.
