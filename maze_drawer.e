note
	description: "A Maze Drawing tool that uses LIST_GRAPH as its underlying implementation."
	author: "CD"
	date: "August 2019"

class
	MAZE_DRAWER

inherit
	ANY
		redefine
			out
		end
create
	make


feature {NONE} -- Initialization

	make (g: LIST_GRAPH [COORDINATE])
			-- create a drawer from graph `g` with size `s`
		require
			bidirectional_edges:
			across g.edges as edge all
					g.edges.has (edge.item.reverse_edge)
				end
			only_cardinal_monospaced_edges:
				across g.edges as edge all
					valid_monospaced_edge (edge.item)
				end -- The only edges allowed are between coordinates one away in N, E, S, W directions
			valid_vertices:
				across g.vertices as v all
					valid_vertex (v.item, g.vertices[g.vertices.count].item.row)
				end
			includes_all_vertices: graph_has_all_vertices (g, g.vertices[g.vertices.count].item.row)
		do
			maze_graph := g
			added_edges := g.edges -- Store edges query here for better performance
			size := g.vertices[g.vertices.count].item.row -- size is based on the final coordinate in the graph
			create maze_ascii.make_filled (" ", (size * 2) + 1, size)
			create player_coord.make([2, 1])
				-- Initialize player to the first player space
			last_coord := player_coord.deep_twin
			draw_graph
			init_player
		end

feature -- Attributes

	maze_graph: LIST_GRAPH [COORDINATE]
		-- The underlying data model of the coordinates and paths in the maze

	added_edges: ARRAY [EDGE [COORDINATE]]
		-- The valid paths in the maze

	size: INTEGER
		-- The size of the maze (size x size coordinates)

	maze_ascii: ARRAY2 [STRING]
		-- The String representation of the maze.

	du: DIRECTION_UTILITY
		-- A utility object converting between directions and tuples

	player_coord: COORDINATE
		-- The current coordinate of the player in `maze_ascii`

	last_coord: like player_coord
		-- last coordinate of the player in `maze_ascii`

feature -- Commands

	init_player
			-- initialize player position in 1,1
		do
			maze_ascii[1,1] := "+-" + path_char.out + "-"
			maze_ascii[2,1] := "| "+ player_char.out + " "
		end

	move_player(d: TUPLE[row_mod: INTEGER; col_mod: INTEGER])
			-- Basic implementation of player movement in the maze.
			-- Move the player with a y_delta = `row_mod` and x_delta = `col_mod`
		do
			last_coord := player_coord.deep_twin -- update last coord
			maze_ascii[player_coord.row, player_coord.col][3] := path_char
			if d ~ du.n or d ~ du.s then -- If north or south
				player_coord := [player_coord.row + d.row_mod, player_coord.col + d.col_mod]
				maze_ascii[player_coord.row, player_coord.col][3] := path_char
				player_coord := [player_coord.row + d.row_mod, player_coord.col + d.col_mod]
				maze_ascii[player_coord.row, player_coord.col][3] := player_char
			else -- if east or west
				if d ~ du.e then
					player_coord := [player_coord.row + d.row_mod, player_coord.col + d.col_mod]
					maze_ascii[player_coord.row, player_coord.col][1] := path_char
					maze_ascii[player_coord.row, player_coord.col][3] := player_char
				else
					maze_ascii[player_coord.row, player_coord.col][1] := path_char -- Replace intermediate position with *
					player_coord := [player_coord.row + d.row_mod, player_coord.col + d.col_mod] -- Move the player
					maze_ascii[player_coord.row, player_coord.col][3] := player_char -- Update new player position with X
				end
			end
			check_win -- Check if we have reached the last spot
		end

	check_win
			-- If the player has won, then they will leave through the exit
		do
			if player_coord.row ~ size * 2 and player_coord.col ~ size then
				maze_ascii[player_coord.row, player_coord.col].at (3) := path_char
				player_coord := [player_coord.row + 1, player_coord.col]
				maze_ascii[player_coord.row, player_coord.col].at (3) := player_char
			end
		end




feature -- Queries

	is_valid_coordinate (coord: COORDINATE): BOOLEAN
			-- Is `coord` a valid coordinate in the maze space?
		do
			if coord.row >= 1 and coord.row <= size and coord.col >= 1 and coord.col <= size then
				Result := True
			else
				Result := False
			end
		end

	is_empty: BOOLEAN
			-- Is the graph empty?
		do
			Result := maze_graph.is_empty
		end

	out: STRING
			-- Return a string representation of the graph
		do
			create Result.make_empty
			Result.append ("%N")
			if not maze_ascii.is_empty then
				across
					1 |..| maze_ascii.height is l_row
				loop
					Result.append ("  ") -- Left padding
					across
						1 |..| maze_ascii.width is l_col
					loop
						Result.append (maze_ascii [l_row, l_col])
					end
				end
			end
		end

feature {NONE} -- Maze Drawing Helper Methods

	draw_graph
			-- Draw the generated maze structure (the walls) and store in graph_printout
		local
			drawn: ARRAY [VERTEX [COORDINATE]]
			q: LINKED_QUEUE [VERTEX [COORDINATE]]
			test_coord: COORDINATE
		do
				-- Fill entire board
			across
				1 |..| maze_ascii.height is l_row
			loop
				across
					1 |..| maze_ascii.width is l_col
				loop
					if (l_row \\ 2) /~ 0 then -- if an odd row
						maze_ascii [l_row, l_col] := "+---"
						if l_col ~ maze_ascii.width then -- if at the last node in the row
							maze_ascii [l_row, l_col] := "+---+%N"
						end
					else -- an even row
						maze_ascii [l_row, l_col] := "|   "
						if l_col ~ maze_ascii.width then -- if at the last node in the row
							maze_ascii [l_row, l_col] := "|   |%N"
						end
					end
				end
			end

				-- Cut holes for entrance/exit
			maze_ascii [1, 1] := "+- -"
			maze_ascii [size * 2 + 1, size] := "+- -+%N"

				-- Break walls for edges in graph
			create q.make
			q.compare_objects
			create drawn.make_empty
			drawn.compare_objects
			across
				maze_graph as v
			loop
				if not drawn.has (v.item) then
					from
						q.extend (v.item)
					until
						q.is_empty
					loop
						across
							1 |..| 4 as opt
						loop
							create test_coord.make ([q.item.item.row + du.num_dir (opt.item).row_mod, q.item.item.col + du.num_dir (opt.item).col_mod])
							if is_valid_coordinate (test_coord) and added_edges.has ([q.item, create {VERTEX [COORDINATE]}.make (test_coord)]) then
								if not drawn.has (create {VERTEX [COORDINATE]}.make (test_coord)) and not q.has (create {VERTEX [COORDINATE]}.make (test_coord)) then
									q.extend (create {VERTEX [COORDINATE]}.make (test_coord))
								end
									-- Choose the right wall to break
								break_wall (q.item.item, opt.item)
							end
						end
						drawn.force (q.item, drawn.count + 1)
						q.remove
					end
				end
			end
		end

	break_wall (coord: COORDINATE; dir: INTEGER)
			-- Helper method for draw_graph to make holes where an edge exists
			-- Break the wall in `dir` direction from coordinate `coord`
		local
			printer_coord: COORDINATE
		do
			printer_coord := [coord.row * 2, coord.col]
			inspect dir
			when 1 then -- Break north wall
				if printer_coord.col ~ maze_ascii.width then
					maze_ascii [printer_coord.row - 1, printer_coord.col] := "+   +%N"
				else
					maze_ascii [printer_coord.row - 1, printer_coord.col] := "+   "
				end
			when 2 then -- Break east wall
				if printer_coord.col + 1 ~ maze_ascii.width then
					maze_ascii [printer_coord.row, printer_coord.col + 1] := "    |%N"
				else
					maze_ascii [printer_coord.row, printer_coord.col + 1] := "    "
				end
			when 3 then -- Break south wall
				if printer_coord.col ~ maze_ascii.width then
					maze_ascii [printer_coord.row + 1, printer_coord.col] := "+   +%N"
				else
					maze_ascii [printer_coord.row + 1, printer_coord.col] := "+   "
				end
			else -- Break west wall
				if printer_coord.col ~ maze_ascii.width then
					maze_ascii [printer_coord.row, printer_coord.col] := "    |%N"
				else
					maze_ascii [printer_coord.row, printer_coord.col] := "    "
				end
			end
		end

feature -- Constants

	path_char: CHARACTER
			-- The character chosen to represent the path the player takes
		once
			Result := '*'
		end

	player_char: CHARACTER
			-- The character chosen to represent the player
		once
			Result := 'X'
		end

	soln_char: CHARACTER
			-- The character chosen to represent the solution path
		once
			Result := 'o'
		end

feature -- Methods for erasing last moves  (Not needed for lab 3)

	remove_chars_from_last_movement
			-- Removes the path characters from the last movement
		local
			last_move_dir: TUPLE[row_mod: INTEGER; col_mod: INTEGER]
		do
			maze_ascii[last_coord.row, last_coord.col][3] := ' ' -- remove the last player's pos

			last_move_dir := calc_last_move_dir
			if last_move_dir ~ du.n then -- The last movement was N
				maze_ascii[last_coord.row - 1, last_coord.col][3] := ' '
			elseif last_move_dir ~ du.e then -- The last movement was E
				maze_ascii[player_coord.row, player_coord.col][1] := ' '
			elseif last_move_dir ~ du.s then -- The last movement was S
				maze_ascii[last_coord.row + 1, last_coord.col][3] := ' '
			elseif last_move_dir ~ du.w then -- The last movement was W
				maze_ascii[last_coord.row, last_coord.col][1] := ' '
			end

		end

	calc_last_move_dir: TUPLE[row_mod: INTEGER; col_mod: INTEGER]
			-- Calculate the direction of the most recent move based on player's current coordinate
			-- and their previous coordinate.
		do
			if last_coord.row < player_coord.row then
				Result := du.s
			elseif last_coord.row > player_coord.row then
				Result := du.n
			elseif last_coord.row = player_coord.row and last_coord.col < player_coord.col then
				Result := du.e
			else
				Result := du.w
			end
		end


feature {NONE} -- Helper methods for invariants and pre/post conditions

	valid_monospaced_edge (e: EDGE [COORDINATE]): BOOLEAN
			-- Ensure the edge `e` is between two vertices that are spaced apart by
			-- exactly 1 cardinal directional movement
		local
			tmp_v: VERTEX [COORDINATE]
		do
			Result := False
			across
				du.dir_arr as d
			loop
				create tmp_v.make ([e.source.item.row + d.item.row_mod, e.source.item.col + d.item.col_mod])
				if e.destination ~ tmp_v then
					Result := True
				end
			end
		end

	valid_vertex (v: VERTEX [COORDINATE]; s: INTEGER): BOOLEAN
			-- Ensure this is a valid vertex in the LIST_GRAPH
		do
			Result := v.item.row > 0 and v.item.row <= s and v.item.col > 0 and v.item.col <= s
		end

	graph_has_all_vertices (g: LIST_GRAPH [COORDINATE]; s: INTEGER): BOOLEAN
			-- Ensure the graph `g` is complete
		do
			Result := across 1 |..| s as i all
						across 1 |..| s as j all
							g.has_vertex (create {VERTEX [COORDINATE]}.make ([i.item, j.item]))
					  	end
					  end
		end

invariant
	bidirectional_edges:
		not is_empty implies
			across maze_graph.edges as edge all maze_graph.edges.has (edge.item.reverse_edge) end
	only_cardinal_monospaced_edges:
			-- The only edges allowed are between coordinates one away in N, E, S, W directions
		not is_empty implies
			across maze_graph.edges as edge all valid_monospaced_edge (edge.item) end
	valid_vertices:
		not is_empty implies
			across maze_graph.vertices as v all valid_vertex (v.item, size) end
	includes_all_vertices:
		not is_empty implies
			graph_has_all_vertices (maze_graph, size)
end
