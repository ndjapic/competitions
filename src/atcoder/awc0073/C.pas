program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
type
	TBooth = record
		x, s: int32;
	end;

function CompareBooths(constref Left, Right: TBooth): Integer;
begin
	Result := CompareValue(Left.x, Right.x);
end;

function ReadAndSort(n: int32): TList<TBooth>;
var
	i: int32;
	b: TBooth;
begin
	randomize;
	Result := TList<TBooth>.Create;
	Result.Capacity := n;
	for i := 0 to n-1 do begin
		ReadLn(b.x, b.s);
		Result.Add(b);
		Result.Exchange(i, Random(i+1));
	end;
	Result.Sort(TComparer<TBooth>.Construct(CompareBooths));
end;

var
	n, d, i, j: int32;
	s, ans: int64;
	booths: TList<TBooth>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);
	booths := ReadAndSort(n);

	ans := 0;

	i := 0;
	s := 0;
	for j := 0 to n-1 do begin
		while abs(booths[i].x - booths[j].x) > d do begin
			dec(s, booths[i].s);
			inc(i);
		end;
		inc(ans, s * booths[j].s);
		inc(s, booths[j].s);
	end;

	writeln(ans);
	booths.Free;
end.
