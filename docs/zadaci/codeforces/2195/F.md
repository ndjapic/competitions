# Задатак: F.pas

```pascal
program _F;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Math;
const
	nn = 3000;
var
	notc, tci, n, i, j, k: int32;
	a, b, c: array [1 .. nn] of int64;
	deg, topo, dl, dr: array [1 .. nn] of int32;
	InArrows, OuArrows: array [1 .. nn] of TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function Positive(a, b, c: int64): boolean;
begin
	if c <= 0 then
		Positive := false
	else if a = 0 then
		Positive := b = 0
	else
		Positive := b*b - 4*a*c < 0;
end;

function Less(i, j: int32): boolean;
begin
	Less := Positive(a[j]-a[i], b[j]-b[i], c[j]-c[i]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do begin
			readln(a[i], b[i], c[i]);
			InArrows[i] := TList<int32>.Create;
			OuArrows[i] := TList<int32>.Create;
			for j := 1 to i-1 do
				if Less(j, i) then begin
					InArrows[i].Add(j);
					OuArrows[j].Add(i);
				end else if Less(i, j) then begin
					InArrows[j].Add(i);
					OuArrows[i].Add(j);
				end;
		end;

		for i := 1 to n do deg[i] := InArrows[i].Count;

		for k := 1 to n do begin
			i := 1;
			while deg[i] <> 0 do inc(i);
			topo[k] := i;
			for j in OuArrows[i] do dec(deg[j]);
			deg[i] := -1;
			dl[i] := -1;
			for j in InArrows[i] do dl[i] := max(dl[i], dl[j]);
			inc(dl[i]);
		end;

		for k := n downto 1 do begin
			i := topo[k];
			dr[i] := -1;
			for j in OuArrows[i] do dr[i] := max(dr[i], dr[j]);
			inc(dr[i]);
		end;

		for i := 1 to n do begin
			write(dl[i] + 1 + dr[i]);
			if i < n then write(' ');
			InArrows[i].Free;
			OuArrows[i].Free;
		end;
		writeln;

	end;
end.

```
