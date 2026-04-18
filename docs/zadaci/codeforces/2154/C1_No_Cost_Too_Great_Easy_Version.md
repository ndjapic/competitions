# Задатак: C1_No_Cost_Too_Great_Easy_Version.pas

```pascal
program C1_No_Cost_Too_Great_Easy_Version;
{$MODE DELPHI}{$INLINE ON}
uses
	sysutils, classes, math;
const
	nn = 200 * 1000 + 1;
var
	notc, tci, n, i, p, x: int32;
	ans: int64;
	e: int8;
	sl: TStringList;
	ios, s: string;
	a, b, c0, c1, t: array [1 .. nn] of int32;
	sieve: array [1 .. nn] of array of int32;

begin
	sl := TStringList.Create;
	sl.Delimiter := ' ';

	for x := 1 to nn do begin
		c0[x] := 0;
		c1[x] := 0;
		t[x] := 0;
		setlength(sieve[x], 1);
	end;

	for p := 2 to nn do
		if t[p] = 0 then begin
			x := p;
			while x <= nn do begin
				if length(sieve[x]) = t[x] then
					setlength(sieve[x], 2 * t[x]);
				sieve[x][t[x]] := p;
				inc(t[x]);
				inc(x, p);
			end;
		end;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		readln(ios);
		sl.DelimitedText := ios;
		i := 0;
		for s in sl do begin
			inc(i);
			a[i] := StrToInt(s);
		end;

		readln(ios);
		sl.DelimitedText := ios;
		i := 0;
		for s in sl do begin
			inc(i);
			b[i] := StrToInt(s);
		end;

		ans := 2;
		for i := 1 to n do begin

			x := a[i];

			for e := 0 to t[x] - 1 do begin
				p := sieve[x][e];
				inc(c0[p]);
				if c0[p] > 1 then ans := 0;
				if (c1[p] > 0) and (c0[p] > 0) then
					ans := min(ans, 1);
			end;

			for e := 0 to t[x+1] - 1 do begin
				p := sieve[x+1][e];
				inc(c1[p]);
				if (c1[p] > 0) and (c0[p] > 0) then
					ans := min(ans, 1);
			end;

		end;

		writeln(ans);

		for i := 1 to n do begin

			x := a[i];

			for e := 0 to t[x] - 1 do begin
				p := sieve[x][e];
				dec(c0[p]);
			end;

			for e := 0 to t[x+1] - 1 do begin
				p := sieve[x+1][e];
				dec(c1[p]);
			end;

		end;

	end;

	FreeAndNil(sl);
end.

```
