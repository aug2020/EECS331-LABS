note
	description: "A class to represent comparable coordinates in a Maze game."
	author: "CD"
	date: "August 2019"

class
	COORDINATE

inherit

	COMPARABLE
		redefine
			is_equal
		end

create
	make

convert
	make ({TUPLE [INTEGER, INTEGER]})

feature {NONE} -- Initialization

	make (c: TUPLE [row: INTEGER; col: INTEGER])
		do
			row := c.row
			col := c.col
		end

feature -- Attributes

	row: INTEGER

	col: INTEGER

feature -- Queries

	is_less alias "<" (other: COORDINATE): BOOLEAN
			-- Compare by row first, then by column. Is Current less than other?
		do
			if Current.row < other.row then
				Result := True
			else
				if Current.row ~ other.row and Current.col < other.col then
					Result := True
				else
					Result := False
				end
			end
		end

	is_equal (other: COORDINATE): BOOLEAN
			-- is Current equal to other?
		do
			if Current.row ~ other.row and Current.col ~ other.col then
				Result := True
			else
				Result := False
			end
		end

end
