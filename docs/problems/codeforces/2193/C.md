# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
var
	notc, tci, n, q, i, j, l, r: int32;
	a, b, s: TList<int32>;
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
	s := TList<int32>.Create;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, q);

		a.Clear;
		readln(ios);
		sl.DelimitedText := ios;
		for str in sl do a.Add(StrToInt(str));

		b.Clear;
		readln(ios);
		sl.DelimitedText := ios;
		for str in sl do b.Add(StrToInt(str));

		for i := n-1 downto 0 do begin
			a[i] := max(a[i], b[i]);
			if i < n-1 then a[i] := max(a[i], a[i+1]);
		end;

		s.Clear;
		s.Add(0);
		for i := 0 to n-1 do s.Add(s[i] + a[i]);

		sl.Clear;
		for j := 1 to q do begin
			readln(l, r);
			sl.Add(IntToStr(s[r] - s[l-1]));
		end;

		writeln(sl.DelimitedText);
		flush(StdErr); flush(output); // DO NOT REMOVE

	end;

	FreeAndNil(sl);
	FreeAndNil(a);
	FreeAndNil(b);
	FreeAndNil(s);
end.

```
