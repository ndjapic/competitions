# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}
uses
	Generics.Collections;
const
	mm = 100;
type
	THeapComparer<_T> = class
		class function Compare(constref Left, Right: _T): SizeInt;
	end;
	THeap<_T, _C> = class
	private
		FItems: TList<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;

var
	n, m, i, j, p1, p2: int8;
	ch1, ch2: char;
	a: array [1 .. mm] of string;
	wins, ranklist: array [1 .. mm] of int8;
	pq: THeap<int32, THeapComparer<int32>>;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref Left, Right: _T): SizeInt;
begin
	Result := - wins[Left] + wins[Right];
	if Result = 0 then
		Result := Left - Right;
end;

// THeap<_T, _C>
function THeap<_T, _C>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (_C.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

function THeap<_T, _C>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;

function THeap<_T, _C>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

constructor THeap<_T, _C>.Create;
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	{FItems.OwnsObjects := False;}
end;

destructor THeap<_T, _C>.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

procedure THeap<_T, _C>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Count;
	FItems.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T, _C>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := FItems[0];
	FItems[0] := FItems[Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	FItems.Delete(Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < Count) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

// Glavni programski blok
begin
	readln(n, m);
	pq := THeap<int32, THeapComparer<int32>>.Create;
	try

		for i := 1 to 2*n do begin
			readln(a[i]);
			wins[i] := 0;
			ranklist[i] := i;
		end;

		for j := 1 to m do begin

			for i := 1 to n do begin
				p1 := ranklist[2*i-1];
				p2 := ranklist[2*i];
				ch1 := a[p1][j];
				ch2 := a[p2][j];

				if (ch1 = 'G') and (ch2 = 'C') then
					inc(wins[p1])
				else if (ch1 = 'C') and (ch2 = 'P') then
					inc(wins[p1])
				else if (ch1 = 'P') and (ch2 = 'G') then
					inc(wins[p1])
				else if ch1 <> ch2 then 
					inc(wins[p2]);
			end;

			for i := 1 to 2*n do pq.Push(i);
			for i := 1 to 2*n do ranklist[i] := pq.Pop;

		end;

		for i := 1 to 2*n do writeln(ranklist[i]);

	finally
		pq.Free;
	end;
end.

```
