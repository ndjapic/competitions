program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 500 * 1000;
	prime = 998244353;
var
	n, i, w: int32;
	s, t: int64;
	fact, invf: array [0 .. nn] of int32;
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

function ncr(n, r: int32): int32;
begin
	ncr := modmul(modmul(invf[r], invf[n-r]), fact[n]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	fact[0] := 1;
	for n := 1 to nn do fact[n] := modmul(n, fact[n-1]);
	invf[nn] := modpow(fact[nn], prime - 2);
	for n := nn downto 1 do invf[n-1] := modmul(n, invf[n]);

	readln(n);

	t := 0;
	for i := n div 2 to n-1 do
		inc(t, ncr(n-1, i));
	t := t mod prime;

	s := 0;
	for i := 1 to n do begin
		read(w);
		inc(s, w);
	end;
	readln;
	s := s mod prime;

	writeln(s * t mod prime);
end.
