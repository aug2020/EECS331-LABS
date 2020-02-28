note
	description: "[
	  Creates LIST_GRAPH[COORDINATE] objects that 
	  represent a maze. 
	  Uses a fixed seed to generate a variety of graph 
	  representations of mazes by sequential calls to:
	  	generate_new_maze (size, level: INTEGER): LIST_GRAPH [COORDINATE]
	  		level 1 (7 x7), level 2 (11 x 11) and level 3 (15x 15)
	]"
	author: "CD"
	date: "August 2019"

class
	MAZE_GENERATOR

create
	make

feature {NONE} -- Initialization

	make
		do
			create rand.make
			rand.set_seed (seed)
		end

feature -- Generator

	generate_new_maze (level: INTEGER): LIST_GRAPH [COORDINATE]
			-- Main function that generates the LIST_GRAPH
			-- representation of a new maze.
		local
			added_edges: ARRAY [EDGE [COORDINATE]]
			new_edge: EDGE [COORDINATE]
			missed_edges: INTEGER
			size: INTEGER
		do
			size := maze_size(level)
				-- Create vertices in the graph
			create Result.make_empty
			create added_edges.make_empty
			added_edges.compare_objects
			Result := blank_maze (size)


				-- Add edges into the graph
			across
				1 |..| size as row
			loop
				across
					1 |..| size as col
				loop
						-- find next valid edge based on random generator
					new_edge := next_edge ([row.item, col.item], size, added_edges)
					if attached new_edge as n_e then
						Result.add_edge (n_e)
						Result.add_edge (n_e.reverse_edge)
						added_edges.force (n_e, added_edges.count + 1)
						added_edges.force (n_e.reverse_edge, added_edges.count + 1)
					else
						missed_edges := missed_edges + 1
					end
				end
			end

				-- Add additional edges to increase connectedness based on difficulty
			inspect level
			when 1 then
				across
					1 |..| 7 as i
				loop
					new_edge := generate_additional_edge (size, added_edges)
					Result.add_edge (new_edge)
					Result.add_edge (new_edge.reverse_edge)
					added_edges.force (new_edge, added_edges.count + 1)
					added_edges.force (new_edge.reverse_edge, added_edges.count + 1)
				end
			when 2 then
				across
					1 |..| 15 as i
				loop
					new_edge := generate_additional_edge (size, added_edges)
					Result.add_edge (new_edge)
					Result.add_edge (new_edge.reverse_edge)
					added_edges.force (new_edge, added_edges.count + 1)
					added_edges.force (new_edge.reverse_edge, added_edges.count + 1)
				end
			else
				across
					1 |..| 30 as i
				loop
					new_edge := generate_additional_edge (size, added_edges)
					Result.add_edge (new_edge)
					Result.add_edge (new_edge.reverse_edge)
					added_edges.force (new_edge, added_edges.count + 1)
					added_edges.force (new_edge.reverse_edge, added_edges.count + 1)
				end
			end
		end


feature {NONE} -- Attributes

	rand: RANDOM

feature {NONE} -- Queries

	maze_size(level: INTEGER): INTEGER
		require
			valid_level: level ~ 1 or level ~ 2 or level ~ 3
		do
			inspect level
			when 1 then
				Result := level_one_size -- 7x7
			when 2 then
				Result := level_two_size -- 11x11
			else
				Result := level_three_size -- 15x15
			end
		end

	next_dir: INTEGER
			-- Randomly selects a new cardinal direction (N,E,S,W).
		do
			Result := rand.item \\ 4
			rand.forth
				-- 0 = N; 1 = E; 2 = S; 3 = W
		ensure
			Result >= 0 and Result <= 3
		end

	next_rand_coord_val (size: INTEGER): INTEGER
			-- Randomly selects a new coordinate based on the difficulty.
		do
			Result := rand.item \\ size
			rand.forth
		end

	generate_additional_edge (size: INTEGER; added_edges: ARRAY [EDGE [COORDINATE]]): EDGE [COORDINATE]
			-- Helper method that generates additional edges.
		local
			found_edge: BOOLEAN
			additional_edge: detachable EDGE [COORDINATE]
			coord: TUPLE [x: INTEGER; y: INTEGER]
			x_i, y_i: INTEGER
		do
			from
				found_edge := false
				x_i := next_rand_coord_val (size)
				y_i := next_rand_coord_val (size)
				coord := [1 + x_i, 1 + y_i]
			until
				found_edge
			loop
				additional_edge := next_edge (coord, size, added_edges)
				if attached additional_edge as a_e then
					found_edge := True
				end
				x_i := next_rand_coord_val (size)
				y_i := next_rand_coord_val (size)
				coord := [1 + x_i, 1 + y_i]
			end
			check attached additional_edge as a_e then
				Result := a_e
			end
		end

	blank_maze (size: INTEGER): LIST_GRAPH [COORDINATE]
			-- Helper method that creates a correctly sized LIST_GRAPH without any edges for a given size.
		local
			v: VERTEX [COORDINATE]
			c: COORDINATE
		do
			create Result.make_empty
			across
				1 |..| size as row
			loop
				across
					1 |..| size as col
				loop
					create c.make ([row.item, col.item])
					create v.make (c)
					Result.add_vertex (v)
				end
			end
		end

	next_edge (coord: TUPLE [x: INTEGER; y: INTEGER]; size: INTEGER; added_edges: ARRAY [EDGE [COORDINATE]]): detachable EDGE [COORDINATE]
			-- Creates the next edge to insert into the maze.
			-- Nothing is returned if a coord is selected which cannot add any more valid edges.
		local
			c: COORDINATE
			du: DIRECTION_UTILITY
			random, i: INTEGER
			test_coord: COORDINATE
			test_edge: EDGE [COORDINATE]
			found_valid_edge: BOOLEAN
		do
			create c.make (coord)
			random := next_dir
			from
				i := 1;
				found_valid_edge := false
			until
				found_valid_edge or else i = 4
			loop
				create test_coord.make ([c.row + du.num_dir (((i + random) \\ 4) + 1).row_mod, c.col + du.num_dir (((i + random) \\ 4) + 1).col_mod])
				create test_edge.make_from_tuple ([create {VERTEX [COORDINATE]}.make (c), create {VERTEX [COORDINATE]}.make (test_coord)])
				if not found_valid_edge
				   and is_valid_coordinate (size, test_coord)
				   and not added_edges.has (test_edge)
				   and degree_count (test_edge, added_edges) < 4 then
					found_valid_edge := True
					Result := test_edge
				end
				i := i + 1
			end
		end

	degree_count (test_edge: EDGE [COORDINATE]; added_edges: ARRAY [EDGE [COORDINATE]]): INTEGER
			-- Returns the count of edges that end at the destination of the test_edge
		do
			Result := 0
			across
				added_edges as edge
			loop
				if edge.item.destination ~ test_edge.destination then
					Result := Result + 1
				end
			end
		end

	is_valid_coordinate (size: INTEGER; coord: COORDINATE): BOOLEAN
			-- is coord a valid coordinate given the specified difficulty?
		do
			if coord.row >= 1 and coord.row <= size and coord.col >= 1 and coord.col <= size then
				Result := True
			else
				Result := False
			end
		end

feature {NONE} -- Constants

	seed: INTEGER -- Seed for RNG. DO NOT CHANGE.
		once
			Result := 501
		end

	level_one_size: INTEGER -- Size of an 'easy' game
		once
			Result := 7
		end

	level_two_size: INTEGER -- Size of a 'medium' game
		once
			Result := 11
		end

	level_three_size: INTEGER -- Size of a 'hard' game
		once
			Result := 15
		end

end
