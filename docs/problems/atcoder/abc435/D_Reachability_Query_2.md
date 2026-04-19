# Problem: D_Reachability_Query_2.pas

```pascal
program D_Reachability_Query_2;
uses
	math;
const
	nn = 300 * 1000;
var
	n, m, q, i, u, v: int32;
	tp: int8;
	dsu, size: array [1 .. nn] of int32;
	black: array [1 .. nn] of boolean;

function find(v: int32): int32;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	find := dsu[v];
end;

procedure union2(u, v: int32);
begin
	dsu[v] := u;
	inc(size[u], size[v]);
	if black[v] then black[u] := true;
end;

procedure union1(u, v: int32);
begin
	u := find(u);
	v := find(v);
	if u = v then
	else if size[u] > size[v] then
		union2(u, v)
	else
		union2(v, u);
end;

begin
	readln(n, m);

	for v := 1 to n do begin
		dsu[v] := v;
		size[v] := 1;
		black[v] := false;
	end;

	for i := 1 to m do begin
		readln(u, v);
		union1(u, v);
	end;

	readln(q);
	for i := 1 to q do begin
		readln(tp, v);
		case tp of

			1: black[find(v)] := true;

			2: if black[find(v)] then
				writeln('Yes')
			else
				writeln('No');

		end;
	end;
end.

```
