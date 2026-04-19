# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults;
type
	THashSet<_T> = record
	private
		const
			HASH_SIZE = 1 shl 3;
			MASK = HASH_SIZE - 1;
			MAX_NODES = 3;
		type
			TNode = record
				Value: _T;
				Next: int32;
			end;
	private
		HashTable: array [0 .. MASK] of int32;
		Pool: array [1 .. MAX_NODES] of TNode;
		FCount: int32;
		Comparer: IEqualityComparer<_T>;
		function GetHash(const Item: _T): int32; inline;
	public
		procedure Init;
		procedure Add(const Item: _T);
		function Contains(const Item: _T): boolean;
		property Count: int32 read FCount;
	end;

function THashSet<_T>.GetHash(const Item: _T): int32;
begin
	result := Comparer.GetHashCode(Item) and MASK;
end;

procedure THashSet<_T>.Init;
begin
	FCount := 0;
	FillChar(HashTable, SizeOf(HashTable), 0);
	Comparer := TEqualityComparer<_T>.Default;
end;

function THashSet<_T>.Contains(const Item: _T): boolean;
var
	idx: int32;
	found: boolean;
begin
	idx := HashTable[GetHash(Item)];
	found := false;

	while (idx <> 0) and not found do
		if Comparer.Equals(Pool[idx].Value, Item) then
			found := true
		else
			idx := Pool[idx].Next;

	result := found;
end;

procedure THashSet<_T>.Add(const Item: _T);
var
	h: int32;
begin
	if not Contains(Item) then begin
		h := GetHash(Item);
		Inc(FCount);
		Pool[FCount].Value := Item;
		Pool[FCount].Next := HashTable[h];
		HashTable[h] := FCount;
	end;
end;

const
	nn = 4;
	n = 3;
var
	i: int8;
	s: string;
	con: array [1 .. nn] of string = ('ABC', 'ARC', 'AGC', 'AHC');
	hs: THashSet<string>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	hs.Init;
	for i := 1 to n do begin
		readln(s);
		hs.Add(s);
	end;

	for i := 1 to nn do
		if not hs.Contains(con[i]) then
			writeln(con[i]);
end.

```
