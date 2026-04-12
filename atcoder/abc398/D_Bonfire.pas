program D_Bonfire;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HASH_SIZE = 1 shl 18;
	MASK = HASH_SIZE - 1;
	MAX_NODES = 200 * 1000;
type
	TNode = record
		x, y: int32;
		next: int32;
	end;
var
	n, i, j, k, r, c: int32;
	s, a: string;
	HashTable: array [0 .. MASK] of int32;
	Pool: array [1 .. MAX_NODES] of TNode;
	PoolPtr: int32 = 0;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function GetHash(x, y: int32): int32;
begin
	result := (int32(uint32(x) * 73856093) xor int32(uint32(y) * 19349663)) and MASK;
end;

procedure Add(x, y: int32);
var
	h: int32;
begin
	h := GetHash(x, y);
	inc(PoolPtr);
	Pool[PoolPtr].x := x;
	Pool[PoolPtr].y := y;
	Pool[PoolPtr].next := HashTable[h];
	HashTable[h] := PoolPtr;
end;

function Contains(x, y: int32): boolean;
var
	idx: int32;
begin
	idx := HashTable[GetHash(x, y)];
	result := false;
	while (idx <> 0) and not result do begin
		result := (Pool[idx].x = x) and (Pool[idx].y = y);
		idx := Pool[idx].next;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, r, c);
	readln(s);
	setlength(a, n);

	i := 0;
	j := 0;
	fillchar(HashTable, sizeof(HashTable), 0);
	Add(i, j);

	for k := 1 to n do begin
		case s[k] of
			'N': begin inc(r); inc(i); end;
			'W': begin inc(c); inc(j); end;
			'S': begin dec(r); dec(i); end;
			'E': begin dec(c); dec(j); end;
		end;

		Add(i, j);

		if Contains(r, c) then
			a[k] := '1'
		else
			a[k] := '0';
	end;

	writeln(a);
end.
