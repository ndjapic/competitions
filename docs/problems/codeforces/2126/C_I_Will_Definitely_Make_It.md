# Problem: C_I_Will_Definitely_Make_It.pas

```pascal
program C_I_Will_Definitely_Make_It;
{$MODE DELPHI}{$H+}{$INLINE ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes;
type
	TIntComparer = class(TComparer<int32>)
		function Compare(constref Left, Right: int32): Integer; override;
	end;
var
	ntc, tci, n, k, i, height, me, water: int32;
	makeit: boolean;
	Split: TStringList;
	Line: string;
	h: TList<int32>;

function TIntComparer.Compare(constref Left, Right: int32): Integer;
begin
	Result := Left - Right;
end;

begin
	randomize;
	Split := TStringList.Create;
	Split.Clear;
	Split.Delimiter := ' ';
	h := TList<int32>.Create(TIntComparer.Create);

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);

		readln(Line);
		Split.DelimitedText := Line;

		h.Clear;
		for i := 1 to n do begin
			height := StrToInt(Split[i-1]);
			h.Add(height);
			h.Exchange(i-1, random(i));
			if i = k then me := height;
		end;
		h.Sort;

		water := 1;
		i := n-1;
		while h[i] > me do dec(i);

		makeit := true;
		while makeit and (h[i] < h[n-1]) do begin
			inc(water, h[i+1] - h[i]);
			makeit := water <= h[i] + 1;
			inc(i);
		end;

		if makeit then
			writeln('YES')
		else
			writeln('NO');

	end;

	FreeAndNil(Split);
	FreeAndNil(h);
end.

```
