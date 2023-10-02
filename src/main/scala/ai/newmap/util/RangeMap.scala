package ai.newmap.util

import scala.collection.immutable.SortedMap

// TODO: improve the implementation - which is going to be key for newmap itself!
case class RangeMap[V](val map: SortedMap[Long, V] = SortedMap.empty[Long, V]) {
  def this(startingKey: Long, startingValue: V) = this(SortedMap(startingKey -> startingValue))

  def insert(key: Long, value: V): RangeMap[V] = {
    new RangeMap(map + (key -> value))
  }

  def get(key: Long): Option[V] = {
    map.to(key).lastOption.map(_._2)
  }

  def valuesUpTo(key: Long): Iterable[V] = {
    map.to(key).values
  }

  def valuesBetween(startKey: Long, endKey: Long): Iterable[V] = {
    map.from(startKey + 1).to(endKey).values
  }
}

object RangeMap {
  // Secondary constructor for creating a RangeMap with a starting key and value
  def apply[V](startingKey: Long, startingValue: V): RangeMap[V] = {
    new RangeMap[V](SortedMap(startingKey -> startingValue))
  }
}