program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #pair #sort #bisect
uses
	Generics.Collections, Generics.Defaults, Math;
const
	NN = 200 * 1000;
type
	TCloth = record
		l, r: int32;
	end;
var
	n, k, i, bl, br, bm, x, noc: int32;
	cloth: TCloth;
	cloths: TList<TCloth>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ClothCompare(constref lhs, rhs: TCloth): int32;
begin
	Result := CompareValue(lhs.r, rhs.r);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, k);

	cloths := TList<TCloth>.Create;
	for i := 1 to n do begin
		readln(cloth.l, cloth.r);
		cloths.Add(cloth);
		cloths.Exchange(i-1, Random(i));
	end;
	cloths.Sort(TComparer<TCloth>.Construct(ClothCompare));

	bl := 0;
	br := 1 shl 30;
	while br - bl > 1 do begin
		bm := (bl + br) div 2;
		noc := 0;
		x := 0;
		i := 0;

		while (i < n) and (noc < k) do
			if cloths[i].l < x then
				inc(i)
			else begin
				inc(noc);
				x := cloths[i].r + bm;
			end;

		if noc >= k then
			bl := bm
		else
			br := bm;
	end;

	if bl = 0 then bl := -1;
	writeln(bl);
	cloths.Free;
end.
