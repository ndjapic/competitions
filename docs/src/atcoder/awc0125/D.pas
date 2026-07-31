program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	NN = 200 * 1000;
type
	TFruit = record
		p, x: int64;
	end;
var
	n, i, l, r, d, c: int32;
	ans: int64;
	fruit: TFruit;
	fruits: TList<TFruit>;
	s: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function FruitCompare(constref l, r: TFruit): int32;
begin
	Result := CompareValue(l.p, r.p);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, d);

	fruits := TList<TFruit>.Create;
	for i := 0 to n-1 do begin
		readln(c, fruit.p);
		fruit.x := fruit.p - c;
		fruits.Add(fruit);
		fruits.Exchange(i, random(i+1));
	end;
	fruits.Sort(TComparer<TFruit>.Construct(FruitCompare));

	s[0] := 0;
	l := 0;
	ans := low(int64);
	for r := 0 to n-1 do begin
		while fruits[r].p - fruits[l].p > d do inc(l);
		ans := max(ans, s[r] - s[l] + fruits[r].x);
		s[r+1] := s[r] + max(0, fruits[r].x);
	end;

	writeln(ans);
	fruits.Free;
end.
