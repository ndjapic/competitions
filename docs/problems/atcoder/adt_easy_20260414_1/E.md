# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections,
	Generics.Defaults;
var
	n, k, i, ai: int32;
	ans: int64;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	a := TList<int32>.Create;
	for i := 0 to n-1 do begin
		read(ai);
		if ai <= k then begin
			a.Add(ai);
			a.Exchange(a.Count - 1, random(a.Count));
		end;
	end;
	readln;

	ans := int64(k+1) * k div 2;
	if a.Count > 0 then begin
		a.Sort;
		dec(ans, a[0]);

		for i := 1 to a.Count - 1 do
			if a[i-1] < a[i] then
				dec(ans, a[i]);
	end;

	writeln(ans);
	a.Free;
end.

```
