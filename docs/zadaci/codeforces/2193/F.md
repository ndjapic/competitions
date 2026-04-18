# Задатак: F.pas

```pascal
program _F;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
const
	nn = 200 * 1000 + 1;
type
	THouse = class
		x, y: int32;
		constructor Create(ax, ay: int32);
	end;
	THouseComparer = class(TComparer<THouse>)
		function Compare(constref L, R: THouse): Integer; override;
	end;
var
	notc, tci, n, i, x, y, l1, r1, l2, r2, d: int32;
	h: TObjectList<THouse>;
	Comparer: THouseComparer;
	dp: array [0 .. nn] of int64;
	sl: TStringList;
	ios: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

constructor THouse.Create(ax, ay: int32);
begin
	inherited Create;
	x := ax;
	y := ay;
end;

function THouseComparer.Compare(constref L, R: THouse): Integer;
begin
	Result := L.x - R.x;
	if Result = 0 then
		Result := L.y - R.y;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;
	sl := TStringList.Create;
	sl.Delimiter := ' ';
	Comparer := THouseComparer.Create;
	Comparer._AddRef;

	readln(notc);
	for tci := 1 to notc do begin

		read(n);
		h := TObjectList<THouse>.Create(True);
		read(x, y);
		h.Add(THouse.Create(x, y));
		readln(x, y);
		h.Add(THouse.Create(x, y));

		readln(ios);
		sl.DelimitedText := ios;
		for i := 0 to n-1 do begin
			x := StrToInt(sl[i]);
			h.Add(THouse.Create(x, 0));
		end;

		readln(ios);
		sl.DelimitedText := ios;
		for i := 0 to n-1 do begin
			y := StrToInt(sl[i]);
			h[i+2].y := y;
		end;

		h.Sort(Comparer);

		l1 := 0;
		r1 := 0;
		dp[0] := 0;

		while r1 <= n do begin
			l2 := r1 + 1;
			r2 := l2;
			while (r2 <= n) and (h[r2+1].x = h[r2].x) do inc(r2);
			d := h[l2].x - h[l1].x + h[r2].y - h[l2].y;
			dp[l2] := min(
				dp[l1] + abs(h[r2].y - h[l1].y),
				dp[r1] + abs(h[r2].y - h[r1].y)) + d;
			dp[r2] := min(
				dp[l1] + abs(h[l2].y - h[l1].y),
				dp[r1] + abs(h[l2].y - h[r1].y)) + d;
			l1 := l2;
			r1 := r2;
		end;

		writeln(dp[n+1]);
		FreeAndNil(h);

	end;

	FreeAndNil(sl);
	Comparer._Release;
end.

```
