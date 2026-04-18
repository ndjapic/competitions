# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	prime = 998244353;
var
	n, d, i, j, p, w, r: int32;
	s, sw: int64;
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

	readln(n, d);

	s := 0;
	r := 1;
	for i := 1 to n do begin
		read(p);
		inc(s, p);
		r := modmul(r, p);
	end;
	s := s mod prime;
	readln;

	sw := 0;
	for j := 1 to d do begin
		read(w);
		inc(sw, w);
	end;
	sw := sw mod prime;
	readln;

	r := modmul(r, modpow(modmul(modpow(s, prime-2), sw), n));
	writeln(r);
end.

```
