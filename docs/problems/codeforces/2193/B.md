# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
var
	notc, tci, n, i, l, r: int32;
	enu : TList<int32>.TEnumerator;
	p: TList<int32>;
	sl: TStringList;
	ios, s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	sl := TStringList.Create;
	sl.Delimiter := ' ';
	p := TList<int32>.Create;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(ios);
		sl.DelimitedText := ios;

		p.Clear;
		for s in sl do p.Add(StrToInt(s));

		l := 0;
		while (l < n-1) and (p[l] + l = n) do inc(l);

		r := n-1;
		while (r > l) and (p[r] < n-l) do dec(r);

		sl.Clear;
		enu := p.GetEnumerator;
		for i := 0 to p.Count -1 do
			if enu.MoveNext then begin
				if (l <= i) and (i <= r) then
					sl.Add(IntToStr(p[l+r-i]))
				else
					sl.Add(IntToStr(p[i]));
			end;

		writeln(sl.DelimitedText);
		flush(StdErr); flush(output); // DO NOT REMOVE

	end;

	FreeAndNil(sl);
	FreeAndNil(p);
end.

```
