# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
var
	notc, tci, n, i: int32;
	x, ans: int64;
	a, b: TList<int32>;
	s: TList<int64>;
	sl: TStringList;
	ios, str: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	sl := TStringList.Create;
	sl.Delimiter := ' ';
	a := TList<int32>.Create;
	b := TList<int32>.Create;
	s := TList<int64>.Create;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		a.Clear;
		readln(ios);
		sl.DelimitedText := ios;
		for str in sl do a.Add(StrToInt(str));
		a.Sort;

		b.Clear;
		readln(ios);
		sl.DelimitedText := ios;
		for str in sl do b.Add(StrToInt(str));

		s.Clear;
		s.Add(0);
		for i := 0 to n-1 do s.Add(s[i] + b[i]);

		ans := 0;
		for i := 1 to n do
			if s[i] <= n then begin
				x := a[n-s[i]];
				ans := max(ans, x*i);
			end;

		writeln(ans);

	end;

	FreeAndNil(sl);
	FreeAndNil(a);
	FreeAndNil(b);
	FreeAndNil(s);
end.

```
