program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #tree #dfs #minimax
uses
	Generics.Collections, Math;
const
	NN = 3000;
var
	n, d, i, a, b, root: int32;
	aok, tak: int64;
	v, h: array [1 .. NN] of int32;
	tar: array [1 .. 2 * NN] of int32;
	adj: array [1 .. NN] of TList<int32>;
	s, tot: array [1 .. NN] of int64;
	dif: array [1 .. NN, 1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure AddArrow(a, b, i: int32);
begin
	adj[a].Add(i);
	tar[i] := b;
end;

procedure dfs(root, a: int32);
var
	i, b: int32;
begin
	if h[a] >= 0 then begin
		s[a] := v[a];
		if h[a] > 0 then
			for i in adj[a] do begin

				b := tar[i];
				if h[b] < h[a] then begin
					h[b] := h[a] - 1;
					dfs(root, b);
					inc(s[a], s[b]);
					dif[root, i div 2] := s[b];
				end;

			end;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);

	for a := 1 to n do begin
		read(v[a]);
		adj[a] := TList<int32>.Create;
	end;
	readln;

	for i := 1 to n-1 do begin
		readln(a, b);
		AddArrow(a, b, 2*i);
		AddArrow(b, a, 2*i+1);
	end;

	for root := 1 to n do begin
		for a := 1 to n do h[a] := -1;
		h[root] := d;
		dfs(root, root);
		tot[root] := s[root];
	end;

	aok := int64(1) shl 60;
	for i := 1 to n-1 do begin
		tak := 0;
		for root := 1 to n do
			tak := max(tak, tot[root] - dif[root, i]);
		aok := min(aok, tak);
	end;
	writeln(aok);
end.
