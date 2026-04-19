# Problem: HashSet.pas

```pascal
program GenericHashSetTemplate;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults;
type
	THashSet<_T> = record
	private
		const
			HASH_SIZE = 1 shl 18;
			MASK = HASH_SIZE - 1;
			MAX_NODES = 200 * 1000;
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

type
	TCell = record
		r, c: int32;
	end;
var
	CellSet: THashSet<TCell>;
	smoke, target: TCell;
	n, k: int32;
	s, res: string;

begin
	CellSet.Init;

	readln(n, target.r, target.c);
	readln(s);
	SetLength(res, n);

	smoke.r := 0;
	smoke.c := 0;
	CellSet.Add(smoke);

	for k := 1 to n do begin
		case s[k] of
			'N': begin inc(target.r); inc(smoke.r); end;
			'W': begin inc(target.c); inc(smoke.c); end;
			'S': begin dec(target.r); dec(smoke.r); end;
			'E': begin dec(target.c); dec(smoke.c); end;
		end;

		CellSet.Add(smoke);
		if CellSet.Contains(target) then
			res[k] := '1'
		else
			res[k] := '0';
	end;
	writeln(res);
end.

```
