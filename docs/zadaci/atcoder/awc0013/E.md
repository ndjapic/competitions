# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 201;
	inf = 1000;
var
	n, m, source, sink, i, u, v, maxFlow: int16;
	loop: boolean;
	capacity: array[0 .. nn, 0 .. nn] of int16;
	visited: array[0 .. nn] of boolean;

function findPath(curr: int16; flow: int16): int16;
var
	i, pushed: int16;
begin
	if curr = sink then
		findPath := flow
	else begin
		visited[curr] := true;

		for i := 0 to sink do begin
			if (not visited[i]) and (capacity[curr, i] > 0) then begin
				pushed := findPath(i, 1);

				if pushed > 0 then begin
					capacity[curr, i] := capacity[curr, i] - pushed;
					capacity[i, curr] := capacity[i, curr] + pushed;
					findPath := pushed;
					exit;
				end;
			end;
		end;

		findPath := 0;
	end;
end;

begin
	readln(n, m);
	source := 0;
	sink := n + m + 1;

	for u := 0 to sink do
		for v := 0 to sink do capacity[u, v] := 0;

	for u := 1 to n do capacity[source, u] := 1;
	for v := n + 1 to n + m do capacity[v, sink] := 1;

	for u := 1 to n do begin
		read(v);
		while not eoln do begin
			read(v);
			capacity[u, n + v] := 1; { Грана од U_u до V_v }
		end;
		readln;
	end;

	maxFlow := 0;
	loop := true;
	while loop do begin
		for i := 0 to sink do visited[i] := false;
		u := findPath(source, 1);

		if u = 0 then
			loop := false
		else
			maxFlow := maxFlow + u;
	end;
	writeln(maxFlow);
end.

```
