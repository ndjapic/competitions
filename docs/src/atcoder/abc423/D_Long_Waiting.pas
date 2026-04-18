program D_Long_Waiting;
{$MODE DELPHI}
uses
	Generics.Collections, math;
const
	nn = 300 * 1000;
type
	THeapComparer<_T> = class
		class function Compare(constref L, R: _T): SizeInt;
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
	n, k, total, l, r: int32;
	a: array [0 .. nn] of int64;
	b, c: array [0 .. nn] of int32;
	pq: THeap<int32, THeapComparer<int32>>;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref L, R: _T): SizeInt;
begin
	Result := a[L] + b[L] - a[R] - b[R];
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
	readln(n, k);

	pq := THeap<int32, THeapComparer<int32>>.Create;
	try

		total := 0;
		a[0] := 0;
		for r := 1 to n do begin
			readln(a[r], b[r], c[r]);
			a[r] := max(a[r], a[r-1]);
			inc(total, c[r]);
			while total > k do begin
				l := pq.Pop;
				dec(total, c[l]);
				a[r] := max(a[r], a[l] + b[l]);
			end;
			pq.Push(r);
			writeln(a[r]);
		end;

	finally
		pq.Free;
	end;
end.
