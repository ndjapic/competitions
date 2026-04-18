program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
	prime = 998244353;
var
	n, m, i, a, c, p2, ans: int32;
	u, v, dsu, sz: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function leader(a: int32): int32;
begin
	if dsu[dsu[a]] <> dsu[a] then dsu[a] := leader(dsu[a]);
	leader := dsu[a];
end;

procedure merge(i: int32);
var
	a, b: int32;
begin
	a := leader(u[i]);
	b := leader(v[i]);
	if (a <> b) and (c > 2) then begin

		dec(c);
		if sz[a] > sz[b] then begin
			dsu[b] := a;
			inc(sz[a], sz[b]);
		end else begin
			dsu[a] := b;
			inc(sz[b], sz[a]);
		end;

	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	for i := 1 to m do readln(u[i], v[i]);

	for a := 1 to n do begin
		dsu[a] := a;
		sz[a] := 1;
	end;

	c := n;
	for i := m downto 1 do merge(i);

	p2 := 1;
	ans := 0;
	for i := 1 to m do begin

		inc(p2, p2);
		if p2 >= prime then dec(p2, prime);

		if leader(u[i]) <> leader(v[i]) then begin
			inc(ans, p2);
			if ans >= prime then dec(ans, prime);
		end;

	end;

	writeln(ans);
end.
