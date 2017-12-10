package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"math/rand"
	"net/http"
	"os"
	"os/exec"
	"reflect"
	"runtime/debug"
	"strconv"
	"sync"
	"time"
)

type Node struct {
	Port       int
	Address    string
	Blockchain []Block
	MemPool    []Transaction
}

type Block struct {
	Hash string
}

type Transaction struct {
	Hash string
}

func assertEq(exp, got interface{}) {
	if !reflect.DeepEqual(exp, got) {
		debug.PrintStack()
		log.Fatal(fmt.Errorf("Wanted %v; Got %v", exp, got))
	}
}

func (node Node) baseURL(path string) string {
	return fmt.Sprintf("http://localhost:%d%v", node.Port, path)
}

func (node Node) request(path string) []byte {
	url := node.baseURL(path)

	res, err := http.Get(url)
	if err != nil {
		log.Fatal(err)
	}

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		log.Fatal(err)
	}

	return body
}

func (node Node) update() Node {
	body := node.request("")

	err := json.Unmarshal(body, &node)
	if err != nil {
		log.Fatal(err)
	}

	return node
}

func (node Node) boot(other Node) {
	ip := other.baseURL("")
	node.request("/boot?ip=" + ip)
}

func (node Node) mineBlock() {
	node.request("/mine-block")
}

func (node Node) transfer(other Node, amount int) {
	url := fmt.Sprintf("/transfer/%v/%d", other.Address, amount)
	node.request(url)
}

func (node Node) ledger() map[string]int {
	var ledger map[string]int
	body := node.request("/ledger")

	err := json.Unmarshal(body, &ledger)
	if err != nil {
		log.Fatal(err)
	}

	return ledger
}

func spawn(wg *sync.WaitGroup, program string, args ...string) {
	cmd := exec.Command(program, args...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err := cmd.Start()
	if err != nil {
		fmt.Printf("%v\n", err)
	}

	cmd.Wait()
	wg.Done()
}

func initNode(port int) Node {
	node := Node{
		Port: port,
	}

	return node.update()
}

func checkLedgers(nodes ...Node) {
	node1 := nodes[0]
	ledger1 := node1.ledger()

	for _, node := range nodes {
		assertEq(node.ledger(), ledger1)
	}
}

func killPending() {
	// kill pending processes
	cmd := exec.Command("pkill", "-f", "spawn-peanos")
	cmd.Run()
}

func integrationTest() {
	killPending()

	var wg sync.WaitGroup

	go spawn(&wg, "node", "spawn-peanos.js", "--port", "3000")
	go spawn(&wg, "node", "spawn-peanos.js", "--port", "3001")
	go spawn(&wg, "node", "spawn-peanos.js", "--port", "3002")

	wg.Add(3)
	time.Sleep(2 * time.Second)

	node1 := initNode(3000)
	node2 := initNode(3001)
	node3 := initNode(3002)

	fmt.Println(node1.Address)
	fmt.Println(node2.Address)
	fmt.Println(node3.Address)

	node2.boot(node1)
	node1.mineBlock()
	node1.transfer(node2, 40)
	node2.mineBlock()

	time.Sleep(1 * time.Second)

	checkLedgers(node1, node2)

	// new node joins the network
	node3.boot(node2)
	time.Sleep(1 * time.Second)
	checkLedgers(node1, node2, node3)

	node3 = node3.update()
	assertEq(len(node3.Blockchain), 3)

	// node2 transfers to node3
	node2.transfer(node3, 20)
	node1.mineBlock()

	checkLedgers(node1, node2, node3)

	node2 = node2.update()
	assertEq(len(node2.Blockchain), 4)

	ledger2 := node2.ledger()
	// node1 mined 2 blocks (200) and transfered 40
	assertEq(ledger2[node1.Address], 160)
	// node2 received 40, mined a block (100) and transfered 20
	assertEq(ledger2[node2.Address], 120)
	// node3 received 20
	assertEq(ledger2[node3.Address], 20)

	// nodes can't transfer more than they own
	// invalid transactions aren't broadcasted
	node2 = node2.update()
	assertEq(len(node2.MemPool), 0)

	node3.transfer(node1, 50)
	time.Sleep(1 * time.Second)
	node2 = node2.update()
	assertEq(len(node2.MemPool), 0)

	// blocks with multiple transactions
	node1.transfer(node3, 60)
	node2.transfer(node3, 20)
	time.Sleep(1 * time.Second)

	// there should be two transactions in the mempool
	node1 = node1.update()
	assertEq(len(node1.MemPool), 2)
	node1.mineBlock()

	time.Sleep(1 * time.Second)

	ledger3 := node3.ledger()
	// node1 mined 3 blocks (300), transfered 40 and then 60
	assertEq(ledger3[node1.Address], 200)
	// node2 received 40, mined a block (100), transfered 20 and then 20
	assertEq(ledger3[node2.Address], 100)
	// node3 received 20, then 60 and then 20
	assertEq(ledger3[node3.Address], 100)

	fmt.Println("DONE!")
	wg.Wait()
}

func networkTest() {
	// still not working

	killPending()

	var wg sync.WaitGroup

	nodeCount := 10
	nodes := make([]Node, 0)

	wg.Add(nodeCount)

	for i := 0; i < nodeCount; i++ {
		port := 3000 + i
		time.Sleep(500 * time.Millisecond)
		go spawn(&wg, "node", "spawn-peanos.js", "--port", strconv.Itoa(port))
	}

	for i := 0; i < nodeCount; i++ {
		port := 3000 + i
		time.Sleep(500 * time.Millisecond)
		nodes = append(nodes, initNode(port))
	}

	prevPeer := nodes[0]

	for i := 0; i < nodeCount; i++ {
		peer := nodes[rand.Intn(nodeCount-1)]
		nodes[i].boot(peer)
		time.Sleep(500 * time.Millisecond)

		nodes[i].boot(prevPeer)
		time.Sleep(1 * time.Second)

		prevPeer = nodes[i]
	}

	for i := 0; i < nodeCount; i++ {
		nodes[i].mineBlock()
		time.Sleep(1 * time.Second)
	}

	for i := 0; i < nodeCount; i++ {
		nodes[i].update()
		time.Sleep(500 * time.Millisecond)
	}

	checkLedgers(nodes...)

	wg.Wait()
}

func main() {
	// integrationTest()
	networkTest()
}
