# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 15;
var
	n, i, ans: int8;
	c: int32;
	w, group: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(v, trips: int8);
var
	u: int8;
begin
	if v > n then
		ans := min(ans, trips)
	else if trips < ans then begin
		group[v] := 0;
		for u := 1 to v-1 do
			if group[u] > 0 then begin
				inc(group[u], w[v]);
				if group[u] <= c then dfs(v+1, trips);
				dec(group[u], w[v]);
			end;

		group[v] := w[v];
		dfs(v+1, trips + 1);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, c);

	for i := 1 to n do read(w[i]);
	readln;

	ans := n;
	dfs(1, 0);
	writeln(ans);
end.

```
