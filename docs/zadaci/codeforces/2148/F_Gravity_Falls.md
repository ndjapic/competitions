# Задатак: F_Gravity_Falls.pas

```pascal
program F_Gravity_Falls;
{$MODE DELPHI}
uses
	Generics.Collections;
const
	nn = 200 * 1000;
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
	ntc, tci, n, i, j, j0: int32;
	a: array [1 .. nn] of array of int32;
	k, ans: array [1 .. nn] of int32;
	pq: THeap<int32, THeapComparer<int32>>;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref L, R: _T): SizeInt;
var
	j: int32;
begin
	j := j0;
	while (j < k[L]) and (j < k[R]) and (a[L][j] = a[R][j]) do inc(j);
	if (j = k[L]) or (j < k[R]) and (a[L][j] < a[R][j]) then
		Result := -1
	else if (j = k[R]) or (j < k[L]) and (a[R][j] < a[L][j]) then
		Result := 1
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

		readln(n);

		pq := THeap<int32, THeapComparer<int32>>.Create;
		try

			j0 := 0;
			for i := 1 to n do begin
				read(k[i]);
				setlength(a[i], k[i]);
				for j := 0 to k[i]-1 do read(a[i][j]);
				pq.Push(i);
			end;
			readln;

			j := 0;
			while pq.Count > 0 do begin
				i := pq.Pop;
				while j < k[i] do begin
					ans[j] := a[i][j];
					inc(j);
				end;
			end;

			j0 := j;
			for j := 0 to j0-2 do write(ans[j], ' ');
			writeln(ans[j0-1]);

		finally
			pq.Free;
		end;
	end;
end.

```
