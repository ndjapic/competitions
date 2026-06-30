program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unfinished
uses
	generics.collections;
const
	NN = 200 * 1000;
var
	notc, tci, n, i, u, v, x: int32;
	ans, d: int64;
	squares: tdictionary<int32, boolean>;
	a, subt, adj, par: array [1 .. NN] of int32;
	sib, tar: array [-NN .. NN] of int32;
	pre: array [-NN .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure addarrow(u, v, i: int32);
begin
	sib[i] := adj[u];
	adj[u] := i;
	tar[i] := v;
end;

procedure readedges(n, m: int32);
var
	u, v, i: int32;
begin
	for v := 1 to n do adj[v] := 0;

	for i := 1 to m do begin
		readln(u, v);
		addarrow(u, v, i);
		addarrow(v, u, -i);
	end;
end;

procedure dfs(u: int32);
var
	i, v, s: int32;
begin
	i := adj[u];
	s := 0;
	while i <> 0 do begin
		v := tar[i];
		if par[u] <> v then begin
			par[v] := u;
			dfs(v);
			pre[i] := s;
			inc(s, subt[v]);
		end;
		i := sib[i];
	end;
	subt[u] := s + 1;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	squares := tdictionary<int32, boolean>.create;
	for x := 1 to 1000 do squares.AddOrSetValue(sqr(x), true);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for v := 1 to n do read(a[v]);
		readln;

		readedges(n, n-1);

		par[1] := 1;
		dfs(1);

		ans := 0;

		for u := 1 to n do
			if squares.ContainsKey(a[u]) then begin
				i := adj[u];
				while i <> 0 do begin
					v := tar[i];
					if par[u] <> v then begin

						d := subt[u] - pre[i] - subt[v];
						inc(ans, d * subt[v] * pre[i]);
						inc(ans, d * subt[v] * (n - subt[u]));

					end;
					i := sib[i];
				end;
			end;

		writeln(ans);

	end;
	squares.free;
end.
