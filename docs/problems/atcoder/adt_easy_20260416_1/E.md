# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, u, v, c, c2: int32;
	found: boolean;
	adj, deg: array [1 .. nn] of int32;
	sib, tar: array [-nn .. nn] of int32;
	seen: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure addarrow(u, v, i: int32);
begin
	sib[i] := adj[u];
	adj[u] := i;
	tar[i] := v;
	inc(deg[v]);
	if deg[v] = 2 then inc(c2);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for v := 1 to n do begin
		adj[v] := 0;
		seen[v] := false;
		deg[v] := 0;
	end;

	c2 := 0;
	for i := 1 to m do begin
		readln(u, v);
		addarrow(u, v, i);
		addarrow(v, u, -i);
	end;

	c := 0;
	if n = m then begin
		u := 1;
		while not seen[u] do begin
			seen[u] := true;
			inc(c);
			i := adj[u];
			found := false;

			while (i <> 0) and not found do begin
				v := tar[i];
				found := not seen[v];
				i := sib[i];
			end;

			if found then u := v;
		end;
	end;

	if (c = n) and (c2 = n) and (v = 1) then
		writeln('Yes')
	else
		writeln('No');
end.

```
