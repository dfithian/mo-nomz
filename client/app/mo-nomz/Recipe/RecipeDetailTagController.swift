//
//  RecipeDetailTagController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 3/7/22.
//

import UIKit

protocol RecipeTagDelegate {
    func updateTags(tags: [String])
}

class RecipeDetailTagController: UICollectionViewController, UICollectionViewDelegateFlowLayout {
    var tags: [String] = []
    var delegate: RecipeTagDelegate? = nil
    
    let TAGS = 0
    let ADD = 1

    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 2
    }
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        switch section {
        case TAGS: return tags.count
        case ADD: return 1
        default: return 0
        }
    }
    
    private func add(_ tag_: String) {
        tags.append(tag_)
        delegate?.updateTags(tags: tags)
        collectionView.reloadData()
    }
    
    private func delete(_ index: Int) {
        tags.remove(at: index)
        delegate?.updateTags(tags: tags)
        collectionView.reloadData()
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        switch indexPath.section {
        case TAGS:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "tagItem", for: indexPath) as! OneCellButton
            cell.button.setTitle(tags[indexPath.row], for: .normal)
            cell.button.menu = UIMenu(children: [
                UIAction(title: "Delete", attributes: .destructive, handler: { _ in
                    self.delete(indexPath.row)
                })
            ])
            cell.button.showsMenuAsPrimaryAction = true
            return cell
        case ADD:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "addItem", for: indexPath) as! OneCellButton
            let actions = Database.getAllTags().filter({ !tags.contains($0) }).map({
                (tag_) in UIAction(title: tag_, handler: { _ in
                    self.add(tag_)
                })
            }) + [UIAction(title: "Add tag", handler: { _ in
                self.promptGetInput(title: "Add tag", completion: self.add)
            })]
            cell.button.menu = UIMenu(children: actions)
            cell.button.showsMenuAsPrimaryAction = true
            return cell
        default:
            return UICollectionViewCell()
        }
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let text: String
        switch indexPath.section {
        case TAGS:
            text = tags[indexPath.row]
            break
        default:
            text = ""
            break
        }
        let font = UIFont.systemFont(ofSize: 12)
        let fontAttributes = [NSAttributedString.Key.font: font]
        let buttonWidth = 25.0
        let width = (text as NSString).size(withAttributes: fontAttributes as [NSAttributedString.Key : Any]).width + buttonWidth
        let height = view.frame.size.height - 10
        return CGSize(width: width, height: height)
    }
}