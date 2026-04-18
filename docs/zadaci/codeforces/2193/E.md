# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
const
	inf = 1 shl 30;
var
	notc, tci, n, i, u: int32;
	v: int64;
	a, b, ans: TList<int32>;
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
	ans := TList<int32>.Create;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		a.Clear;
		ans.Clear;
		readln(ios);
		sl.DelimitedText := ios;
		for str in sl do begin
			a.Add(StrToInt(str));
			ans.Add(inf);
		end;
		a.Sort;

		for u in a do ans[u-1] := 1;

		i := 0;
		while (i < n) and (a[i] = 1) do inc(i);

		b.Clear;
		if i = 0 then begin
			b.Add(a[0]);
			i := 1;
		end;

		while i < n do begin
			if a[i-1] < a[i] then b.Add(a[i]);
			inc(i);
		end;

		for u := 1 to n do
			if ans[u-1] < inf then begin
				i := 0;
				if i < b.Count then v := int64(u) * b[i];
				while (i < b.Count) and (v <= n) do begin
					ans[v-1] := min(ans[v-1], ans[u-1] + 1);
					inc(i);
					if i < b.Count then v := int64(u) * b[i];
				end;
			end;

		sl.Clear;
		for u := 1 to n do
			if ans[u-1] < inf then
				sl.Add(IntToStr(ans[u-1]))
			else
				sl.Add('-1');
		writeln(sl.DelimitedText);

	end;

	FreeAndNil(sl);
	FreeAndNil(a);
	FreeAndNil(b);
	FreeAndNil(ans);
end.

```
