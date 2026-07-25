program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 500 * 1000;
	PRIME = 998244353;
var
	n, i, ans: int32;
	a: array [1 .. NN] of int32;
	b, h: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function modmul(a, b: int32): int32;
begin
	modmul := int64(a) * b mod PRIME;
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
	if a >= PRIME then dec(a, PRIME);
end;

procedure moddec(var a: int32; b: int32);
begin
	dec(a, b);
	if a < 0 then inc(a, prime);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	h[0] := 0;
	for i := 1 to n do begin
		read(a[i]);
		h[i] := h[i-1];
		modinc(h[i], modpow(i, PRIME - 2));
	end;
	readln;

	b[0] := 0;
	ans := 0;
	for i := 1 to n do begin
		if i-1 <= n-i then begin
			b[i] := b[i-1];
			modinc(b[i], h[n+1-i]);
			moddec(b[i], h[i-1]);
		end else
			b[i] := b[n+1-i];
		modinc(ans, modmul(a[i], b[i]));
	end;

	writeln(ans);
end.
