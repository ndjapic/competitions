# Problem: C_Double_Perspective.pas

```pascal
program C_Double_Perspective;
const
	nn = 300 * 1000;
	nn2 = 600 * 1000;
var
	ntc, tci, n, i, k, v: int32;
	a, b, ans: array [1 .. nn] of int32;
	dsu, size: array [1 .. nn2] of int32;

function find(v: int32): int32;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	find := dsu[v];
end;

procedure union2(u, v: int32);
begin
	dsu[v] := u;
	inc(size[u], size[v]);
end;

procedure union1(i, u, v: int32);
begin
	u := find(u);
	v := find(v);

	if u <> v then begin
		inc(k);
		ans[k] := i;
		if size[u] > size[v] then
			union2(u, v)
		else
			union2(v, u);
	end;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		k := 0;

		for v := 1 to 2*n do begin
			dsu[v] := v;
			size[v] := 1;
		end;

		for i := 1 to n do begin
			readln(a[i], b[i]);
			union1(i, a[i], b[i]);
		end;

		writeln(k);
		for i := 1 to k-1 do write(ans[i], ' ');
		writeln(ans[k]);

	end;
end.

```
