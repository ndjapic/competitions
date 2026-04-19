# Problem: D_Arboris_Contractio.pas

```pascal
program D_Arboris_Contractio;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000;
var
	ntc, tci: int16;
	n, i, u, v, mx, ans: int32;
	adj: array [1 .. nn] of TList<int32>;
	par, clist: array [1 .. nn] of int32;

procedure dfs(u: int32);
var
	v: int32;
begin
	for v in adj[u] do
		if par[u] <> v then begin
			par[v] := u;
			dfs(v);
			inc(clist[u], clist[v]);
		end;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		try

			for v := 1 to n do adj[v] := TList<int32>.Create;

			for i := 1 to n-1 do begin
				readln(u, v);
				adj[u].Add(v);
				adj[v].Add(u);
			end;

			u := 1;
			for v := 1 to n do begin

				if adj[v].Count > adj[u].Count then u := v;

				if adj[v].Count = 1 then
					clist[v] := 1
				else
					clist[v] := 0;

			end;

			ans := 0;
			mx := 0;
			for v in adj[u] do
				if clist[v] = 0 then begin
					par[v] := u;
					dfs(v);
					inc(ans, clist[v]);
				end else
					mx := 1;

			writeln(ans + mx - 1);

		finally
			for v := 1 to n do adj[v].Free;
		end;
	end;
end.

```
