program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math, generics.collections;
const
	NN = 200 * 1000;
	PRIME = 998244353;
var
	n, i, num, den, ans: int32;
	p: array [2 .. NN] of int32;
	c, d: array [1 .. NN] of int32;
	adj: array [1 .. NN] of tlist<int32>;
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

procedure modinc(var a: int32; b: int32);
begin
	inc(a, b);
	if a >= prime then dec(a, prime);
end;

procedure moddec(var a: int32; b: int32);
begin
	dec(a, b);
	if a < 0 then inc(a, prime);
end;

procedure dfs(i: int32);
var
	j, r: int32;
begin
	for j in adj[i] do begin
		dfs(j);
		modinc(c[i], max(0, c[j]));
	end;

	moddec(c[i], d[i]);
	for r := 1 to d[i] do begin
		num := modmul(num, r + c[i]);
		den := modmul(den, r);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(n);

	for i := 1 to n do adj[i] := tlist<int32>.create;

	for i := 2 to n do begin
		read(p[i]);
		adj[p[i]].add(i);
	end;
	readln;

	for i := 1 to n do begin
		read(c[i]);
		modinc(c[i], 0);
	end;
	readln;

	for i := 1 to n do begin
		read(d[i]);
		modinc(d[i], 0);
	end;
	readln;

	num := 1;
	den := 1;
	dfs(1);
	ans := modmul(num, modpow(den, PRIME - 2));
	writeln(ans);

	for i := 1 to n do adj[i].free;
end.
