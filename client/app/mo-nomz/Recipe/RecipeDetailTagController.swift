//
//  RecipeDetailTagController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 3/7/22.
//

import UIKit

protocol RecipeTagDelegate {
    func updateRecipeTags(tags: [String])
}

class RecipeDetailTagController: UICollectionViewController, UICollectionViewDelegateFlowLayout {
    var tags: [String] = []
    var delegate: RecipeTagDelegate? = nil
    
    let HEADER = 0
    let TAGS = 1
    let ADD = 2

    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 3
    }
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        switch section {
        case HEADER: return 1
        case TAGS: return tags.count
        case ADD: return 1
        default: return 0
        }
    }
    
    private func add(_ tag_: String) {
        tags.append(tag_)
        delegate?.updateRecipeTags(tags: tags)
        collectionView.reloadData()
    }
    
    private func delete(_ index: Int) {
        tags.remove(at: index)
        delegate?.updateRecipeTags(tags: tags)
        collectionView.reloadData()
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        switch indexPath.section {
        case HEADER:
            return collectionView.dequeueReusableCell(withReuseIdentifier: "tagHeader", for: indexPath)
        case TAGS:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "tagItem", for: indexPath) as! OneCellButton
            cell.button.setTitle(tags[indexPath.row], for: .normal)
            cell.button.menu = UIMenu(children: [
                UIAction(title: "Remove tag", image: UIImage(systemName: "xmark"), attributes: .destructive, handler: { _ in
                    self.delete(indexPath.row)
                })
            ])
            cell.button.showsMenuAsPrimaryAction = true
            return cell
        case ADD:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "addItem", for: indexPath) as! OneCellButton
            let actions = Database.getTopTags().filter({ !tags.contains($0) }).map({
                (tag_) in UIAction(title: tag_, handler: { _ in
                    self.add(tag_)
                })
            }) + [UIAction(title: "Add tag", image: UIImage(systemName: "plus"), handler: { _ in
                self.promptGetInput(title: "Add tag", content: nil, completion: self.add)
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
        case HEADER:
            text = "Tags:"
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
