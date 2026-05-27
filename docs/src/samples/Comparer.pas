program Program_Comparer;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
type
	TBlock = record
		h, w: int32;
	end;

function CompareBlocks(constref Left, Right: TBlock): Integer;
begin
	Result := - Sign(int64(Left.h * Left.w) - int64(Right.h * Right.w));
end;

function ReadAndSort(n: int32): TList<TBlock>;
var
	i: int32;
	b: TBlock;
begin
	Result := TList<TBlock>.Create;
	Result.Capacity := n;
	for i := 0 to n-1 do begin
		ReadLn(b.h, b.w);
		Result.Add(b);
		Result.Exchange(i, Random(i+1));
	end;
	{ReadLn;}
	Result.Sort(TComparer<TBlock>.Construct(CompareBlocks));
end;

var
	n, i: int32;
	b: TBlock;
	blocks: TList<TBlock>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n);

	{blocks := ReadAndSort(n);}

	blocks := TList<TBlock>.Create;
	blocks.Capacity := n;

	for i := 1 to n do begin
		ReadLn(b.h, b.w);
		blocks.Add(b);
	end;

	blocks.Sort(TComparer<TBlock>.Construct(CompareBlocks));

	for i := 0 to blocks.Count - 1 do
		Writeln(blocks[i].h, ' ', blocks[i].w);

	blocks.Free;
end.
