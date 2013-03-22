//  Helper module for example applications.
//
//  Author: iano <scaly.iano@gmail.com>

package main

import (
	"container/list"
	"fmt"
	"reflect"
)

const (
	HEARTBEAT_LIVENESS = 3 //  3-5 is reasonable
)

//  Golang implementation of zlist in C
type ZList struct {
	list.List
}

func NewList() *ZList {
	return new(ZList)
}

func (self *ZList) Delete(value interface{}) {
	for elem := self.Front(); elem != nil; elem = elem.Next() {
		if reflect.DeepEqual(elem.Value, value) {
			self.Remove(elem)
			break
		}
	}
}

func (self *ZList) Pop() *list.Element {
	if self.Len() == 0 {
		return nil
	}
	elem := self.Front()
	self.Remove(elem)
	return elem
}

//  Print messages neatly
func Dump(msg [][]byte) {
	for _, part := range msg {
		isText := true
		fmt.Printf("[%03d] ", len(part))
		for _, char := range part {
			if char < 32 || char > 127 {
				isText = false
				break
			}
		}
		if isText {
			fmt.Printf("%s\n", part)
		} else {
			fmt.Printf("%X\n", part)
		}
	}
}
