# Problem: E_Cut_in_Half.pas

```pascal
program E_Cut_in_Half;
{$MODE DELPHI}
uses
	Generics.Collections;
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
	ntc, tci, n, k, x, i: int32;
	ai: extended;
	pq: THeap<extended, THeapComparer<extended>>;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref Left, Right: _T): SizeInt;
begin
	if Left < Right then
		Result := 1
	else if Left > Right then
		Result := -1
	else
		Result := 0;
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

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		pq := THeap<extended, THeapComparer<extended>>.Create;
		try

			readln(n, k, x);
			for i := 1 to n do begin
				read(ai);
				pq.Push(ai);
			end;
			readln;

			for i := 1 to k do begin
				ai := pq.Pop / 2;
				pq.Push(ai);
				pq.Push(ai);
			end;

			for i := 1 to x do ai := pq.Pop;
			writeln(ai:0:10);

		finally
			pq.Free;
		end;

	end;
end.

```
