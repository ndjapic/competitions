program DequeDemo;
{$MODE DELPHI} // Use Delphi mode for class syntax

(* This implementation provides the basic
addFirst, addLast, removeFirst, and removeLast operations
(named AddFront, AddRear, RemoveFront, RemoveRear in the code). *)

uses
	SysUtils; // For ExitCode

type
	// Define a Node class for the doubly linked list
	TDequeNode = class
		Value: Integer;
		Next: TDequeNode;
		Prev: TDequeNode;
		constructor Create(AValue: Integer);
	end;

	TDeque = class
	private
		FFront: TDequeNode;
		FRear: TDequeNode;
		FCount: Integer;
	public
		constructor Create;
		destructor Destroy; override;
		procedure AddFront(AValue: Integer);
		procedure AddRear(AValue: Integer);
		function RemoveFront: Integer;
		function RemoveRear: Integer;
		function PeekFront: Integer;
		function PeekRear: Integer;
		function IsEmpty: Boolean;
		property Count: Integer read FCount;
	end;

// --- TDequeNode Implementation ---

constructor TDequeNode.Create(AValue: Integer);
begin
	inherited Create;
	Value := AValue;
	Next := nil;
	Prev := nil;
end;

// --- TDeque Implementation ---

constructor TDeque.Create;
begin
	inherited Create;
	FFront := nil;
	FRear := nil;
	FCount := 0;
end;

destructor TDeque.Destroy;
var
	Curr: TDequeNode;
	Temp: TDequeNode;
begin
	Curr := FFront;
	while Curr <> nil do begin
		Temp := Curr.Next;
		Curr.Free;
		Curr := Temp;
	end;
	inherited Destroy;
end;

function TDeque.IsEmpty: Boolean;
begin
	Result := FCount = 0;
end;

procedure TDeque.AddFront(AValue: Integer);
var
	NewNode: TDequeNode;
begin
	NewNode := TDequeNode.Create(AValue);
	if IsEmpty then begin
		FFront := NewNode;
		FRear := NewNode;
	end else begin
		NewNode.Next := FFront;
		FFront.Prev := NewNode;
		FFront := NewNode;
	end;
	Inc(FCount);
end;

procedure TDeque.AddRear(AValue: Integer);
var
	NewNode: TDequeNode;
begin
	NewNode := TDequeNode.Create(AValue);
	if IsEmpty then begin
		FFront := NewNode;
		FRear := NewNode;
	end else begin
		NewNode.Prev := FRear;
		FRear.Next := NewNode;
		FRear := NewNode;
	end;
	Inc(FCount);
end;

function TDeque.RemoveFront: Integer;
var
	Temp: TDequeNode;
begin
	if IsEmpty then
		raise Exception.Create('Deque is empty');

	Result := FFront.Value;
	Temp := FFront;
	FFront := FFront.Next;
	if FFront <> nil then
		FFront.Prev := nil
	else
		FRear := nil;
	Temp.Free;
	Dec(FCount);
end;

function TDeque.RemoveRear: Integer;
var
	Temp: TDequeNode;
begin
	if IsEmpty then
		raise Exception.Create('Deque is empty');

	Result := FRear.Value;
	Temp := FRear;
	FRear := FRear.Prev;
	if FRear <> nil then
		FRear.Next := nil
	else
		FFront := nil;
	Temp.Free;
	Dec(FCount);
end;

function TDeque.PeekFront: Integer;
begin
	if IsEmpty then
		raise Exception.Create('Deque is empty');
	Result := FFront.Value;
end;

function TDeque.PeekRear: Integer;
begin
	if IsEmpty then
		raise Exception.Create('Deque is empty');
	Result := FRear.Value;
end;

// --- Example Usage ---

var
	MyDeque: TDeque;
	Value: Integer;
begin
	MyDeque := TDeque.Create;
	try
		MyDeque.AddFront(10);
		MyDeque.AddRear(20);
		MyDeque.AddFront(5);

		Writeln('Front element is: ', MyDeque.PeekFront); // 5
		Writeln('Rear element is: ', MyDeque.PeekRear);	 // 20
		Writeln('Deque size: ', MyDeque.Count);

		Value := MyDeque.RemoveFront;
		Writeln('Removed from front: ', Value); // 5

		Value := MyDeque.RemoveRear;
		Writeln('Removed from rear: ', Value);	 // 20

		Writeln('Is Deque empty? ', MyDeque.IsEmpty);

		Value := MyDeque.RemoveFront;
		Writeln('Removed from front: ', Value); // 10

		Writeln('Is Deque empty? ', MyDeque.IsEmpty);

	except
		on E: Exception do
			Writeln('Error: ', E.Message);
	end;
	MyDeque.Destroy;
end.

