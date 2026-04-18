# Задатак: D_Segments_Covering.pas

```pascal
program D_Segments_Covering;
{$INLINE ON}
const
	nn = 200 * 1000;
	prime = 998244353;
var
	ntc, tci, n, m, i, l, r, p, q, x, y: int32;
	ll, prob, last, prev: array [1 .. nn] of int32;
	pre, dp: array [0 .. nn] of int32;

function modmul(a, b: int32): int32; inline;
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

procedure modinc(var a: int32; b: int32); inline;
begin
    inc(a, b);
    if a >= prime then dec(a, prime);
end;

procedure moddec(var a: int32; b: int32); inline;
begin
    dec(a, b);
    if a < 0 then inc(a, prime);
end;

begin
	{readln(ntc);}
	ntc := 1;
	for tci := 1 to ntc do begin

		readln(n, m);

		for r := 1 to m do last[r] := 0;
		for r := 0 to m do pre[r] := 1;

		for i := 1 to n do begin
			readln(l, r, p, q);

			ll[i] := l;
			p := modmul(p, modpow(q, prime - 2));
			prob[i] := p;
			pre[r] := modmul(pre[r], 1 - p + prime);

			prev[i] := last[r];
			last[r] := i;
		end;

		for r := 1 to m do pre[r] := modmul(pre[r-1], pre[r]);

		dp[0] := 1;
		for r := 1 to m do begin
			dp[r] := 0;
			i := last[r];

			while i > 0 do begin
				l := ll[i];
				p := prob[i];

				(*
				* pre[r] / pre[l-1] / (1-p) * p * dp[l-1]
				* *)
				x := modmul(p, pre[r]);
				x := modmul(x, dp[l-1]);
				y := modmul(1-p+prime, pre[l-1]);

				modinc(dp[r], modmul(x, modpow(y, prime - 2)));
				i := prev[i];
			end;
		end;

		writeln(dp[m]);

	end;
end.

```
