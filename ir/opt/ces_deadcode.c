
/*
 * transforms add( const, add( projP, projM)) 
 * transforms add( const, add( projM, projP)) 

 * transforms add( add( projP, projM), const) 
 * transforms add( add( projM, projP), const) 


 * to         add( projP, add( projM, const))
 * transforms 
 * to         base + c1*x + c2*y
 * return TRUE if successful
 *        FALSE if different structure
 */
/*
int ces_transform_var_memop(ir_node* add){
	ir_node* constant = get_Add_left(add);
	ir_node* add2 = get_Add_right(add);

	if ( !is_Const(constant) ) {
		constant = get_Add_right(add);
		add2 = get_Add_left(add);

		if ( !is_Const(constant) ) {
			DBG((dbg, LEVEL_DEFAULT, "transforming %+F\n",add));
			DBG((dbg, LEVEL_DEFAULT, "const expected, got %+F\n", constant));
			return 0;
		}
	}
	if( !is_Add(add2)) {
		DBG((dbg, LEVEL_DEFAULT, "add expected, got %+F\n", add2));
		return 0; 
	}
	
	ir_node* projP = get_Add_left(add2);
	ir_node* projO = get_Add_right(add2);
	if( !(get_irn_mode(projP) == get_modeP()) ) {
		projP = get_Add_right(add2);
		projO = get_Add_left(add2);
		if( !(get_irn_mode(projP) == get_modeP()) ) {
			DBG((dbg, LEVEL_DEFAULT, "modeP proj expected, got %+F\n", projP));
			//return 0; 
		}
	}
	set_irn_n(add, 0, projP);
	set_irn_n(add, 1, add2);
	set_irn_n(add2, 0, constant);
	set_irn_n(add2, 1, projO);
	
	DBG((dbg, LEVEL_DEFAULT, "\x1b[33;1mtransform success\x1b[0m\n"));
	return 1;
}
*/

/*
ir_node* ces_find_base_v1(ir_node* node, int count, char* text[]) {
	#define MAX_RECURSE 5
	ir_node* res = NULL;

	if( count > MAX_RECURSE){
		DBG((dbg, LEVEL_DEFAULT, "MAX_RECURSE reached\n"));
		return NULL;
	}

	for(int i=0; i < get_irn_arity(node); i++) {
		ir_node* pred= get_irn_n(node, i);
		if( is_Proj(pred) && get_irn_mode(pred) == get_modeP() ) {
			res = pred;
			break;
		} else if( is_Start(pred)) {
				res = NULL;
		} else {
			ir_node* temp = ces_find_base(pred, ++count,text);
			if( temp != NULL){
				res = temp;
				break;
			}
		}
	}
	return res;
}
*/
/*
ir_node* ces_find_base(ir_node* node, int count, char** text) {
	#define MAX_RECURSE 10
	ir_node* res = NULL;
	int close = 0;

	if( count > MAX_RECURSE){
		DBG((dbg, LEVEL_DEFAULT, "MAX_RECURSE reached\n"));
		return NULL;
	}
	
	switch( get_irn_opcode(node) ) {
	case iro_Add:
		*text += sprintf(*text, "add( ");
		close=1;
		break;
	case iro_Const:
		*text += tarval_snprintf (*text, 50, get_Const_tarval(node));
		*text += sprintf(*text,",");
		break;
	case iro_Shl:
		*text += sprintf(*text, "shl(");
		close=1;
		break;
	case iro_Proj:
		if( get_irn_mode(node) == get_modeT() ) {
			return NULL;
		} else
		if( get_irn_mode(node) == get_modeP() ) {
			*text += sprintf(*text, "\x1b[1m0xProjP%li\x1b[0m, ", get_irn_node_nr(node) );
			res = node;
		} else
			*text += sprintf(*text, "0xProj%li, ", get_irn_node_nr(node) );
		break;
	case iro_SymConst:
		*text += sprintf(*text, "0xSymC%li, ", get_irn_node_nr(node) );
		break;
	case iro_Start:
		return NULL;
	default:
		DBG((dbg, LEVEL_DEFAULT, "unhandled opcode:%+F\n",node));
	}

	for(int i=0; i < get_irn_arity(node); i++) {
		ir_node* pred= get_irn_n(node, i);
		ir_node* temp = ces_find_base(pred, ++count,text);
		if( temp != NULL)
			res = temp;
	}
	if( close )
		*text += sprintf(*text, ")");
	return res;
}
*/

/**
 * returns load_ptr->add_right->const-> long unsigned
 * @param a load node which follow the systen above
 * @return the constant found (phi equals 0) or -1
 */
/*
void ces_get_memop_const(ir_node* node, ir_node** base, ir_tarval** offset) {
	assert( is_Load(node) || is_Store(node) );
	ir_node* ptr = is_Load(node)  ? get_Load_ptr(node) : get_Store_ptr(node);
	if( is_Add(ptr) ) {
//		*base = get_Add_left(ptr);
		
		char* calculation= obstack_alloc(ces_obstack, sizeof(char)*255);
		char* text = calculation;
		memset(calculation,255, sizeof(char));
		ir_node* ultimate = ces_find_base(ptr, 0, &text);
		if( ultimate == NULL) {
			DBG((dbg, LEVEL_DEFAULT, "aaaarrgh, could not find base\n"));
		}
			*base = ultimate;
			DBG((dbg, LEVEL_DEFAULT, "%+F formula:%s\n", ptr, calculation));
		
		obstack_free(ces_obstack, calculation);

	  //cannot transform if more than one user of this subtree
//		if( is_Add(*base) && get_Add_left(*base) ) 
//			ces_transform_var_memop(ptr);
		
		ir_node* offset_node = get_Add_right(ptr);
		*offset = ces_get_tarval(offset_node);
	} else if( is_Sel(ptr) ) {
		*base = ptr;
		// return offset 0
		*offset = is_Store(node) ? get_mode_null(get_irn_mode(get_Store_ptr(node))) : get_mode_null(get_irn_mode(node));
//		DBG((dbg, LEVEL_DEFAULT, "memop:%+F, base:%+F, offset:%+F\n", ptr, *base, *offset));
	} else if( is_SymConst(ptr) || is_Proj(ptr) ) {
		*base = ptr;
		// return offset 0
		*offset = is_Store(node) ? get_mode_null(get_irn_mode(get_Store_value(node))) : get_mode_null(get_Load_mode(node));
	} else if( is_Phi(ptr) ) {
		*base = ptr;
		// we assume that if addr-calculation is missing the offset must be 0
		*offset = 0;
	} else {
		DBG((dbg, LEVEL_DEFAULT, "ptr != add||sel: %+F\n",ptr));
		*offset=-1;
		*base=-1;
	}
}
*/

/*
void ces_get_memop_const(ir_node* node, ir_node** base, ir_tarval** offset) {
	assert( is_Load(node) || is_Store(node) );
	ir_node* ptr = is_Load(node)  ? get_Load_ptr(node) : get_Store_ptr(node);
		
	struct load_base* load_base = ir_nodemap_get_fast(ces_load_base, node);
	if( load_base->base  == NULL) {
		DBG((dbg, LEVEL_DEFAULT, "aaaarrgh, could not find base\n"));
	} 
	*base = load_base->base;
	 
	if( is_Add(ptr) ) {
		ir_node* offset_node = get_Add_right(ptr);
		*offset = ces_get_tarval(offset_node);
	} else if( is_Sel(ptr) ) {
		// return offset 0
		*offset = is_Store(node) ? get_mode_null(get_irn_mode(get_Store_ptr(node))) : get_mode_null(get_irn_mode(node));
//		DBG((dbg, LEVEL_DEFAULT, "memop:%+F, base:%+F, offset:%+F\n", ptr, *base, *offset));
	} else if( is_SymConst(ptr) || is_Proj(ptr) ) {
		// return offset 0
		*offset = is_Store(node) ? get_mode_null(get_irn_mode(get_Store_value(node))) : get_mode_null(get_Load_mode(node));
	} else if( is_Phi(ptr) ) {
		// we assume that if addr-calculation is missing the offset must be 0
		*offset = 0;
	} else {
		DBG((dbg, LEVEL_DEFAULT, "ptr != add||sel: %+F\n",ptr));
		*offset=-1;
		*base=-1;
	}
}
*/


/*
int ces_eval_lineq(struct load_base* base, int max) {
	int value=0;
	if( (intptr_t)base->y < 1000)
		value += (intptr_t)base->y*base->c2_value;
	else
		value += base->c2_value;

	if( (intptr_t)base->x < 1000)
		value += (intptr_t)base->x*base->c1_value;
	else
		value += base->c1_value*max;
	return value;
}
*/

/**
 * recursive (depth search) on the IR tree stopping at BB boundaries
 * collects all ir_load nodes in the node_list
 * uses irn_visited to detect loops and multiple references of the same node
 * @param the current node
 * @param list to store the ir_load nodes
 * @return the newly allocated, uninitialized element.
 */
//void ces_find_load(ir_node *node, plist_t* ces_load_list, plist_t* ces_store_list, unsigned int block_id);
/*
void ces_find_load(ir_node *node, plist_t* ces_load_list, plist_t* ces_store_list, unsigned int block_id) {

	if ( irn_visited(node) ) {
		//		DBG((ces_dbg, LEVEL_DEFAULT, "node already visited %N!\n",  node ));
		return;
		//stop recursion
	}
	//else
	mark_irn_visited(node);

	//only examine current basic block
	if ( get_irn_idx( get_irn_n(node, -1) ) != block_id) {
		DBG((ces_dbg, LEVEL_DEFAULT, "out of block node detected %N!\n",  node ));
		//stop recursion here

	} else if ( is_Block(node) ) {
		DBG((ces_dbg, LEVEL_DEFAULT, "block node %N - %t!\n", node, node ));
		//stop recursion here
		;
	} else 	if ( is_Load(node) ) {
		///if load found then add to load_list; break
		DBG((ces_dbg, LEVEL_DEFAULT, "load node %N - %t!\n", node, node ));
		plist_insert_front(ces_load_list, (void *) node);
		//stop recursion here

	} else 	if ( is_Store(node) || is_Return(node) ) {
		///if store found then add to store_list; break
		DBG((ces_dbg, LEVEL_DEFAULT, "store node %N - %t!\n", node, node ));
		plist_insert_front(ces_store_list, (void *) node);
		//stop recursion here

	} else {

		unsigned int arity = get_irn_arity(node);
		//		DBG((ces_dbg, LEVEL_DEFAULT, "node %+F has %u pred \n", node, arity));
		for (unsigned int i = 0; i < arity; ++i) {
			//foreach successor
			////follow until Block begin
			ir_node *pred = get_irn_n(node, i);
			//			DBG((ces_dbg, LEVEL_DEFAULT, "recursion, next node:%N\n", pred ));
			ces_find_load(pred, ces_load_list, ces_store_list, block_id);
		}
	}
}
*/
/* scheduled for removal
ir_tarval* ces_get_tarval(ir_node* node);
int irn_load_same_mem(ir_node* node1, ir_node* node2);
*/

/**
 * compare two load_nodes by their mem reference
 * shows that memops are possibly parallel
 * @param two ir_node
 * @return true if mem references are the same
 */
/*
int irn_load_same_mem(ir_node* node1, ir_node* node2) {
	if (is_memop(node1) && !( is_Store(node1)  || is_Load(node1) ) )
		DBG((ces_dbg, LEVEL_3, "memop &! load | store: %+F\n", node1));
	if (is_memop(node2) && !( is_Store(node2)  || is_Load(node2) ) )
		DBG((ces_dbg, LEVEL_3, "memop &! load | store: %+F\n", node2));


	if (is_Load(node1) || is_Store(node1) || is_Return(node1) ) {
		if( get_irn_node_nr( get_memop_mem(node1) ) == get_irn_node_nr(get_memop_mem(node2)) )
			return 1;
		else
			return 0;
	}
	return -1;
}
*/
/*
ir_tarval* ces_get_tarval(ir_node* node) {
	ir_tarval* tarval = NULL;

	if( is_Const(node) ) {
		tarval = get_Const_tarval(node);
	}
	if( is_Sel(node) ){
		ir_entity* ent = get_Sel_entity(node);
		ir_node* ptr = get_Sel_ptr(node);
		DBG((ces_dbg, LEVEL_3,"Sel has no tarval: ent: %+F, ptr: %+F\n", ent, ptr));
	}
	return tarval;
}
*/
/*
pset* ces_disable_node_deduplication(ir_graph* irg) {
	//clear value_table to disable identify_remember()
	//because identify_remember() merged the copied node with the original
	//moved to calling function!
	pset* value_table = irg->value_table;
	irg->value_table=NULL;
	return value_table;
}

void ces_enable_node_deduplication(ir_graph* irg, pset* value_table) {
	irg->value_table = value_table;
}
*/
