# Задатак: C_Concat_X_th.pas

```pascal
program C_Concat_X_th;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, classes, sysutils;
const
	nn = 100 * 1000;
type
	TIntComparer = class(TComparer<int32>)
		function Compare(constref Left, Right: int32): Integer; override;
	end;
var
	n, k, i, d: int8;
	e: int16;
	x, y: int32;
	s: string;
	L1: TStringList;
	a: array [0 .. 4] of int8;
	keys: array [0 .. nn, 0 .. 6] of qword;
	p: TList<int32>;
	Comparer: TIntComparer;

function TIntComparer.Compare(constref Left, Right: int32): Integer;
var
	e: int8;
begin
	e := 6;
	while (e > 0) and (keys[Left, e] = keys[Right, e]) do dec(e);
	if keys[Left, e] < keys[Right, e] then
		Result := -1
	else if keys[Left, e] > keys[Right, e] then
		Result := 1
	else
		Result := 0;
end;

procedure dfs(j: int8);
var
	i, z: int8;
	e: int16;
begin
	if j = k then begin

		for e := 0 to 6 do keys[y, e] := 0;

		e := 8 * 50;
		for j := 0 to k-1 do begin
			i := a[j];
			for z := 0 to length(L1[i]) - 1 do begin
				inc(keys[y, e shr 6], qword(ord(L1[i][z+1])) shl (e and 63));
				dec(e, 8);
			end;
		end;

		p.Add(y);
		p.Exchange(y, Random(y+1));
		inc(y);

	end else
		for i := 0 to n-1 do begin
			a[j] := i;
			dfs(j+1);
		end;
end;

begin
	randomize;
	readln(n, k, x);

	L1 := TStringList.Create;
	p := TList<int32>.Create;
	Comparer := TIntComparer.Create;
	Comparer._AddRef;

	try

		for i := 0 to n-1 do begin
			readln(s);
			L1.Add(s);
		end;

		y := 0;
		dfs(0);
		p.Sort(Comparer);
		y := p[x-1];

		e := 8*50;
		d := 1;
		while d > 0 do begin
			d := (keys[y, e shr 6] shr (e and 63)) and 255;
			if d > 0 then write(chr(d));
			dec(e, 8);
		end;
		writeln;

	finally
		L1.Free;
		p.Free;
		Comparer._Release;
	end;
end.

```
