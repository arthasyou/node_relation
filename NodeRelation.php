<?php

namespace App;

use Illuminate\Database\Eloquent\Model;
use Illuminate\Support\Facades\DB;

class NodeRelation extends Model
{
    public static function addNode($id, $ancestor)
    {
    	DB::insert("INSERT INTO `node_relations` (`ancestor`, `descendant`, `depth`) SELECT cp.ancestor, $id, cp.depth+1 FROM node_relations AS cp WHERE cp.descendant=$ancestor UNION ALL SELECT $id, $id, 0;");
    }

    public static function getChildren($ancestor)
    {
    	return static::select('descendant')
    	->where('ancestor', '=', $ancestor)
    	->where('depth', 1)
    	->get()->toArray();
    }

    public static function getSelfAndChildren($ancestor)
    {
        return static::select('descendant')
        ->where('ancestor', '=', $ancestor)
        ->whereIn('depth', [0,1])
        ->get()->toArray();
    }

    public static function getDescendant($ancestor)
    {
    	return static::select('descendant')
    	->where('ancestor', $ancestor)
    	->get()->toArray();
    }

    public static function getAncestor($descendant)
    {
        return static::select('ancestor')
        ->where('descendant', $descendant)
        ->get()->toArray();
    }

    public static function removeAllDescendant($ancestor)
    {
    	DB::delete("DELETE a FROM `node_relations` a JOIN `node_relations` b ON (a.descendant = b.descendant) where b.ancestor=$ancestor;");
    }

    public static function removeDescendant($descendant)
    {
        // step 1:
        DB::update("UPDATE `node_relations` a JOIN node_relations b ON (a.`descendant` = b.`descendant`) JOIN `node_relations` c ON (a.`descendant` = c.`descendant` AND a.`ancestor` = c.`ancestor`) SET a.`depth` = c.`depth`-1 WHERE b.`ancestor` = $descendant AND c.`depth` > b.`depth`;");
        // step 2:
        static::where('descendant', $descendant)
        ->orWhere('ancestor', $descendant)
        ->delete();
    }

    public static function moveDescendantToAncestor($id, $ancestor)
    {
    	// step 1:
    	DB::delete("DELETE a FROM `node_relations` AS a JOIN `node_relations` AS d ON a.descendant = d.descendant LEFT JOIN `node_relations` AS x ON x.ancestor = d.ancestor AND x.descendant = a.ancestor WHERE d.ancestor = $id AND x.ancestor is NULL;");
    	// step 2:
    	DB::insert("INSERT INTO `node_relations` (ancestor, descendant, depth) SELECT supertree.ancestor, subtree.descendant, supertree.depth+subtree.depth+1 FROM `node_relations` AS supertree JOIN `node_relations` AS subtree WHERE subtree.ancestor = $id AND supertree.descendant = $ancestor;");
    }

    public function scopeBegats($query, $playerId)
    {
        $summary = NodeRelation::begatsSummary($playerId);
        $query->selectRaw("depth, count(descendant) as amount")
        ->begatsWhere($playerId)
        ->groupBy('depth')
        ->union($summary);  
    }

    public function scopeBegatsWhere($query, $playerId)
    {        
        $query->where('ancestor', $playerId)
        ->where('depth', '<>', 0);             
    }

    public function scopeBegatsSummary($query, $playerId)
    {
        $query->selectRaw("'汇总', count(descendant)")
        ->begatsWhere($playerId);
    }
}
