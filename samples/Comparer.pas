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

var
	n, i: int32;
	b: TBlock;
	blocks: TList<TBlock>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	blocks := TList<TBlock>.Create;
	blocks.Capacity := n;

	for i := 1 to n do begin
		ReadLn(b.h, b.w);
		blocks.Add(b);
	end;

	blocks.Sort(TComparer<TBlock>.Construct(CompareBlocks));

	for i := 0 to blocks.Count - 1 do
		Writeln(blocks[i].h, ' ', blocks[i].w);
end.
