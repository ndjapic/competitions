# Problem: C_New_Skill_Acquired.pas

```pascal
program C_New_Skill_Acquired;
const
	nn = 200 * 1000;
var
	n, i, a, b, ans: int32;
	dsu, size: array [0 .. nn] of int32;

function find(v: int32): int32;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	find := dsu[v];
end;

procedure union(u, v: int32);
begin
	u := find(u);
	v := find(v);
	if u = v then
	else if size[u] > size[v] then begin
		dsu[v] := u;
		inc(size[u], size[v]);
	end else begin
		dsu[u] := v;
		inc(size[v], size[u]);
	end;
end;

begin
	readln(n);

	for a := 0 to n do begin
		dsu[a] := a;
		size[a] := 1;
	end;

	for i := 1 to n do begin
		readln(a, b);

		if (a = 0) and (b = 0) then
			union(i, 0)
		else begin
			union(i, a);
			union(i, b);
		end;
	end;

	ans := 0;
	for a := 1 to n do
		if find(a) = find(0) then inc(ans);

	writeln(ans);
end.

```
